use std::fs;
use std::path::Path;

use anyhow::{Context, Result, ensure};

use crate::{backend, lexer, parser};

fn normalize_output(output: &str) -> String {
    output.replace("\r\n", "\n").trim_end().to_string()
}

#[test]
fn slow_runs_programs_across_backends() -> Result<()> {
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
                        for mut backend in backend::backends() {
                            let result = backend.run(&program);
                            ensure!(
                                result.is_err(),
                                "Expected error for backend {} in {}",
                                backend.name(),
                                path.display()
                            );
                            let error = result.err().unwrap().to_string();
                            ensure!(
                                error.contains(expected_error),
                                "Expected error containing '{expected_error}', got '{error}'"
                            );
                        }
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

        for mut backend in backend::backends() {
            let output = backend.run(&program).with_context(|| {
                format!("Backend {} failed for {}", backend.name(), path.display())
            })?;
            let actual_output = normalize_output(&output);
            assert_eq!(
                actual_output,
                expected_output,
                "Backend {} mismatch for {}",
                backend.name(),
                path.display()
            );
        }
    }

    Ok(())
}
