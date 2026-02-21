use anyhow::{Context, Result, ensure};
use std::fs;
use std::path::Path;

use pyparse::backend::Backend;
use pyparse::backend::interpreter::Interpreter;
use pyparse::backend::jit::JIT;
use pyparse::backend::vm::VM;
use pyparse::{lexer, parser};
use test_support::{
    Case, CaseClass, is_backend_unsupported, load_cases, normalize_output, run_python_file,
    run_python_startup, validate_unsupported_backends,
};

const KNOWN_BACKENDS: [&str; 6] = ["interpreter", "vm", "jit", "cpython", "pypy", "micropython"];

fn parity_required(env_var: &str) -> bool {
    std::env::var(env_var)
        .map(|value| value == "1")
        .unwrap_or(false)
}

fn detect_python_interpreter() -> Result<Option<String>> {
    if let Ok(python) = std::env::var("PYTHON")
        && run_python_startup(&python).is_ok()
    {
        return Ok(Some(python));
    }

    for candidate in ["python3", "python"] {
        if run_python_startup(candidate).is_ok() {
            return Ok(Some(candidate.to_string()));
        }
    }

    if parity_required("PYTHON_PARITY_REQUIRED") {
        anyhow::bail!(
            "CPython parity required but no interpreter found. Set PYTHON or install python3."
        );
    }

    eprintln!("Skipping CPython parity test: no PYTHON env or python3 interpreter found.");
    Ok(None)
}

fn detect_named_interpreter(
    env_var: &str,
    required_var: &str,
    candidates: &[&str],
    display_name: &str,
) -> Result<Option<String>> {
    if let Ok(interpreter) = std::env::var(env_var) {
        if run_python_startup(&interpreter).is_ok() {
            return Ok(Some(interpreter));
        }
        if parity_required(required_var) {
            anyhow::bail!(
                "{} parity required but '{}' is not runnable. Fix {}.",
                display_name,
                interpreter,
                env_var
            );
        }
    }

    for candidate in candidates {
        if run_python_startup(candidate).is_ok() {
            return Ok(Some((*candidate).to_string()));
        }
    }

    if parity_required(required_var) {
        anyhow::bail!(
            "{} parity required but no interpreter found. Set {} or install one of: {}.",
            display_name,
            env_var,
            candidates.join(", ")
        );
    }

    eprintln!(
        "Skipping {} parity test: no interpreter found (set {} or install {}).",
        display_name,
        env_var,
        candidates.join(", ")
    );
    Ok(None)
}

fn detect_pypy_interpreter() -> Result<Option<String>> {
    detect_named_interpreter("PYPY", "PYPY_PARITY_REQUIRED", &["pypy3", "pypy"], "PyPy")
}

fn detect_micropython_interpreter() -> Result<Option<String>> {
    detect_named_interpreter(
        "MICROPYTHON",
        "MICROPYTHON_PARITY_REQUIRED",
        &["micropython"],
        "MicroPython",
    )
}

fn run_python_program(interpreter: &str, interpreter_name: &str, case: &Case) -> Result<String> {
    run_python_file(interpreter, &case.program_path)
        .with_context(|| format!("Running {} for {}", interpreter_name, case.name))
}

fn run_programs_for_backend(backend: &dyn Backend) -> Result<()> {
    let cases = load_cases(Path::new("tests/programs"))?;

    for case in cases {
        validate_unsupported_backends(&case, &KNOWN_BACKENDS)?;
        if is_backend_unsupported(&case, backend.name()) {
            continue;
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

fn run_programs_for_python_backend(
    interpreter: &str,
    backend_name: &str,
    interpreter_name: &str,
) -> Result<()> {
    let cases = load_cases(Path::new("tests/programs"))?;

    for case in cases {
        validate_unsupported_backends(&case, &KNOWN_BACKENDS)?;
        if is_backend_unsupported(&case, backend_name) {
            continue;
        }
        if !matches!(case.spec.class, CaseClass::RuntimeSuccess) {
            continue;
        }
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
        let actual_output =
            normalize_output(&run_python_program(interpreter, interpreter_name, &case)?);
        let expected_output = normalize_output(&expected);
        assert_eq!(
            actual_output, expected_output,
            "Backend {} mismatch for {}",
            backend_name, case.name,
        );
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
fn runs_programs_cpython_backend() -> Result<()> {
    let Some(interpreter) = detect_python_interpreter()? else {
        return Ok(());
    };
    run_programs_for_python_backend(&interpreter, "cpython", "CPython")
}

#[test]
fn runs_programs_pypy_backend() -> Result<()> {
    let Some(interpreter) = detect_pypy_interpreter()? else {
        return Ok(());
    };
    run_programs_for_python_backend(&interpreter, "pypy", "PyPy")
}

#[test]
fn runs_programs_micropython_backend() -> Result<()> {
    let Some(interpreter) = detect_micropython_interpreter()? else {
        return Ok(());
    };
    run_programs_for_python_backend(&interpreter, "micropython", "MicroPython")
}
