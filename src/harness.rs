use std::fs;
use std::path::Path;

use anyhow::{Context, Result, ensure};

use crate::backend::Backend;
use crate::backend::interpreter::Interpreter;
use crate::backend::jit::JIT;
use crate::backend::transpiler::Transpiler;
use crate::backend::vm::VM;
use crate::{lexer, parser};

fn normalize_output(output: &str) -> String {
    output.replace("\r\n", "\n").trim_end().to_string()
}

fn run_programs_for_backend(backend: &dyn Backend) -> Result<()> {
    let programs_dir = Path::new("tests/programs");
    let mut programs = Vec::new();

    for entry in
        fs::read_dir(programs_dir).with_context(|| format!("Reading {}", programs_dir.display()))?
    {
        let path = entry?.path();
        if path.extension().and_then(|ext| ext.to_str()) == Some("py") {
            programs.push(path);
        }
    }

    ensure!(
        !programs.is_empty(),
        "No .py programs found in {}",
        programs_dir.display()
    );
    programs.sort();

    for path in programs {
        let source =
            fs::read_to_string(&path).with_context(|| format!("Reading {}", path.display()))?;
        let tokenized = lexer::tokenize(&source);
        let expected_error_path = path.with_extension("err");
        if expected_error_path.exists() {
            let expected_error = fs::read_to_string(&expected_error_path)
                .with_context(|| format!("Reading {}", expected_error_path.display()))?;
            let expected_error = expected_error.trim();

            match tokenized {
                Err(err) => {
                    let error = err.to_string();
                    ensure!(
                        error.contains(expected_error),
                        "Expected error containing '{expected_error}', got '{error}'"
                    );
                }
                Ok(tokens) => match parser::parse_tokens(tokens) {
                    Ok(program) => {
                        let result = backend.run(&program);
                        ensure!(
                            result.is_err(),
                            "Expected error for backend {} in {}",
                            backend.name(),
                            path.display()
                        );
                        let error = match result {
                            Ok(_) => unreachable!(),
                            Err(error) => error.to_string(),
                        };
                        ensure!(
                            error.contains(expected_error),
                            "Expected error containing '{expected_error}', got '{error}'"
                        );
                    }
                    Err(err) => {
                        let error = err.to_string();
                        ensure!(
                            error.contains(expected_error),
                            "Expected error containing '{expected_error}', got '{error}'"
                        );
                    }
                },
            }
            continue;
        }

        let tokens = tokenized.with_context(|| format!("Tokenizing {}", path.display()))?;
        let expected_path = path.with_extension("out");
        let expected = fs::read_to_string(&expected_path)
            .with_context(|| format!("Reading {}", expected_path.display()))?;
        let program =
            parser::parse_tokens(tokens).with_context(|| format!("Parsing {}", path.display()))?;
        let expected_output = normalize_output(&expected);

        let output = backend
            .run(&program)
            .with_context(|| format!("Backend {} failed for {}", backend.name(), path.display()))?;
        let actual_output = normalize_output(&output);
        assert_eq!(
            actual_output,
            expected_output,
            "Backend {} mismatch for {}",
            backend.name(),
            path.display()
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
fn runs_programs_transpiler_backend() -> Result<()> {
    run_programs_for_backend(&Transpiler)
}
