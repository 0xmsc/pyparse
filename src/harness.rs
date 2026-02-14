use std::fs;
use std::path::Path;
use std::process::Command;

use anyhow::{Context, Result, ensure};
use serde::Deserialize;

use crate::backend::Backend;
use crate::backend::interpreter::Interpreter;
use crate::backend::jit::JIT;
use crate::backend::transpiler::Transpiler;
use crate::backend::vm::VM;
use crate::{lexer, parser};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum CaseClass {
    RuntimeSuccess,
    FrontendError,
    BackendRuntimeError,
    UnsupportedFeature,
}

#[derive(Debug, Deserialize)]
struct BenchConfig {
    enabled: bool,
    tags: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct ExpectedOutcome {
    exit_code: i32,
    stdout_file: Option<String>,
    stderr_file: Option<String>,
    stderr_contains_file: Option<String>,
}

#[derive(Debug, Deserialize)]
struct CaseSpec {
    class: CaseClass,
    parity: bool,
    bench: BenchConfig,
    expected: ExpectedOutcome,
}

#[derive(Debug)]
struct Case {
    name: String,
    dir: std::path::PathBuf,
    program_path: std::path::PathBuf,
    spec: CaseSpec,
}

fn normalize_output(output: &str) -> String {
    output.replace("\r\n", "\n").trim_end().to_string()
}

fn read_text_from_case_file(case: &Case, relative_path: &str) -> Result<String> {
    fs::read_to_string(case.dir.join(relative_path))
        .with_context(|| format!("Reading {} fixture file {}", case.name, relative_path))
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

fn load_cases() -> Result<Vec<Case>> {
    let programs_dir = Path::new("tests/programs");
    let mut cases = Vec::new();

    for entry in
        fs::read_dir(programs_dir).with_context(|| format!("Reading {}", programs_dir.display()))?
    {
        let path = entry?.path();
        if !path.is_dir() {
            continue;
        }

        let case_path = path.join("case.yaml");
        if !case_path.exists() {
            continue;
        }

        let program_path = path.join("program.py");
        ensure!(
            program_path.exists(),
            "Missing program.py for case {}",
            path.display()
        );

        let case_name = path
            .file_name()
            .and_then(|value| value.to_str())
            .map(str::to_string)
            .with_context(|| format!("Invalid case directory name {}", path.display()))?;
        let case_raw = fs::read_to_string(&case_path)
            .with_context(|| format!("Reading {}", case_path.display()))?;
        let spec: CaseSpec = serde_yaml::from_str(&case_raw)
            .with_context(|| format!("Parsing {}", case_path.display()))?;

        cases.push(Case {
            name: case_name,
            dir: path,
            program_path,
            spec,
        });
    }

    ensure!(
        !cases.is_empty(),
        "No test cases found in {}",
        programs_dir.display()
    );
    cases.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(cases)
}

fn run_programs_for_backend(backend: &dyn Backend) -> Result<()> {
    let python_interpreter = detect_python_interpreter()?;

    for case in load_cases()? {
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
                let expected = read_text_from_case_file(&case, stdout_file)?;
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
                let expected_error = read_text_from_case_file(&case, expected_file)?;
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
                let expected_error = read_text_from_case_file(&case, expected_file)?;
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
