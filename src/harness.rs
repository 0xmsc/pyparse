use std::fs;
use std::path::Path;
use std::process::Command;

use anyhow::{Context, Result, ensure};

use crate::backend::Backend;
use crate::backend::interpreter::Interpreter;
use crate::backend::jit::JIT;
use crate::backend::transpiler::Transpiler;
use crate::backend::vm::VM;
use crate::fixtures::{self, Case, CaseClass};
use crate::{lexer, parser};

fn normalize_output(output: &str) -> String {
    output.replace("\r\n", "\n").trim_end().to_string()
}

fn python_parity_required() -> bool {
    std::env::var("PYTHON_PARITY_REQUIRED")
        .map(|value| value == "1")
        .unwrap_or(false)
}

fn detect_python_interpreter() -> Result<Option<String>> {
    if let Ok(python) = std::env::var("PYTHON") {
        let status = Command::new(&python)
            .arg("--version")
            .status()
            .with_context(|| format!("Running '{python} --version'"))?;
        ensure!(
            status.success(),
            "Configured PYTHON interpreter '{}' is not executable",
            python
        );
        return Ok(Some(python));
    }

    let status = Command::new("python3").arg("--version").status();
    if let Ok(status) = status
        && status.success()
    {
        return Ok(Some("python3".to_string()));
    }

    if python_parity_required() {
        anyhow::bail!(
            "CPython parity required but no interpreter found. Set PYTHON or install python3."
        );
    }

    eprintln!("Skipping CPython parity test: no PYTHON env or python3 interpreter found.");
    Ok(None)
}

fn run_python_program(interpreter: &str, case: &Case) -> Result<String> {
    let output = Command::new(interpreter)
        .arg(&case.program_path)
        .output()
        .with_context(|| format!("Running CPython for {}", case.name))?;
    ensure!(
        output.status.success(),
        "CPython failed for {}: {}",
        case.name,
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

fn run_programs_for_backend(backend: &dyn Backend) -> Result<()> {
    let cases = fixtures::load_cases(Path::new("tests/programs"))?;
    let python_interpreter = detect_python_interpreter()?;

    for case in cases {
        if case.spec.parity {
            ensure!(
                matches!(case.spec.class, CaseClass::RuntimeSuccess),
                "Case {} has parity enabled but is not runtime_success",
                case.name
            );
        }
        if case.spec.bench.enabled {
            ensure!(
                !case.spec.bench.tags.is_empty(),
                "Case {} has bench enabled but no tags",
                case.name
            );
        }
        let source = fs::read_to_string(&case.program_path)
            .with_context(|| format!("Reading {}", case.name))?;
        let tokenized = lexer::tokenize(&source);
        match case.spec.class {
            CaseClass::RuntimeSuccess => {
                ensure!(
                    case.spec.expected.exit_code == 0,
                    "Case {} expected exit code must be 0 for runtime_success",
                    case.name
                );
                let stdout_file = case
                    .spec
                    .expected
                    .stdout_file
                    .as_deref()
                    .with_context(|| format!("Missing stdout_file in {}", case.name))?;
                let expected = case.read_text(stdout_file)?;
                let tokens = tokenized.with_context(|| format!("Tokenizing {}", case.name))?;
                let program = parser::parse_tokens(tokens)
                    .with_context(|| format!("Parsing {}", case.name))?;
                let output = backend.run(&program).with_context(|| {
                    format!("Backend {} failed for {}", backend.name(), case.name)
                })?;
                let actual_output = normalize_output(&output);
                let expected_output = normalize_output(&expected);
                assert_eq!(
                    actual_output,
                    expected_output,
                    "Backend {} mismatch for {}",
                    backend.name(),
                    case.name
                );

                if case.spec.parity
                    && let Some(interpreter) = python_interpreter.as_deref()
                {
                    let python_output = run_python_program(interpreter, &case)?;
                    let expected_output = normalize_output(&python_output);
                    assert_eq!(
                        actual_output,
                        expected_output,
                        "Parity mismatch for backend {} in {}",
                        backend.name(),
                        case.name
                    );
                }
            }
            CaseClass::FrontendError => {
                ensure!(
                    case.spec.expected.exit_code == 1,
                    "Case {} expected exit code must be 1 for frontend_error",
                    case.name
                );
                let expected_file = case
                    .spec
                    .expected
                    .stderr_contains_file
                    .as_deref()
                    .or(case.spec.expected.stderr_file.as_deref())
                    .with_context(|| format!("Missing stderr expectation file in {}", case.name))?;
                let expected_error = case.read_text(expected_file)?;
                let expected_error = expected_error.trim();
                match tokenized {
                    Err(error) => {
                        let actual = error.to_string();
                        ensure!(
                            actual.contains(expected_error),
                            "Expected frontend error containing '{expected_error}' in {}, got '{actual}'",
                            case.name
                        );
                    }
                    Ok(tokens) => {
                        let parse_result = parser::parse_tokens(tokens);
                        ensure!(
                            parse_result.is_err(),
                            "Expected frontend error in {}, but parsing succeeded",
                            case.name
                        );
                        let actual = parse_result
                            .expect_err("parse_result checked as err")
                            .to_string();
                        ensure!(
                            actual.contains(expected_error),
                            "Expected frontend error containing '{expected_error}' in {}, got '{actual}'",
                            case.name
                        );
                    }
                }
            }
            CaseClass::BackendRuntimeError => {
                ensure!(
                    case.spec.expected.exit_code == 1,
                    "Case {} expected exit code must be 1 for backend_runtime_error",
                    case.name
                );
                let expected_file = case
                    .spec
                    .expected
                    .stderr_contains_file
                    .as_deref()
                    .or(case.spec.expected.stderr_file.as_deref())
                    .with_context(|| format!("Missing stderr expectation file in {}", case.name))?;
                let expected_error = case.read_text(expected_file)?;
                let expected_error = expected_error.trim();
                let tokens = tokenized.with_context(|| format!("Tokenizing {}", case.name))?;
                let program = parser::parse_tokens(tokens)
                    .with_context(|| format!("Parsing {}", case.name))?;
                let result = backend.run(&program);
                ensure!(
                    result.is_err(),
                    "Expected backend runtime error for backend {} in {}",
                    backend.name(),
                    case.name
                );
                let actual = result.expect_err("result checked as err").to_string();
                ensure!(
                    actual.contains(expected_error),
                    "Expected backend runtime error containing '{expected_error}' in {}, got '{actual}'",
                    case.name
                );
            }
            CaseClass::UnsupportedFeature => {}
        }
    }

    Ok(())
}

#[test]
fn runs_programs_interpreter_backend() -> Result<()> {
    run_programs_for_backend(&Interpreter::new())
}

#[test]
fn runs_programs_vm_backend() -> Result<()> {
    run_programs_for_backend(&VM::new())
}

#[test]
fn runs_programs_jit_backend() -> Result<()> {
    run_programs_for_backend(&JIT::new())
}

#[test]
fn runs_programs_transpiler_backend() -> Result<()> {
    run_programs_for_backend(&Transpiler)
}
