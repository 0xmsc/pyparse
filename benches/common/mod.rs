#![allow(dead_code)]
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use pyparse::ast::Program;
use pyparse::fixtures::{self, CaseClass};
use pyparse::{lexer, parser};

pub fn workloads() -> Vec<(String, String)> {
    let cases =
        fixtures::load_cases(Path::new("tests/programs")).expect("load fixture cases for benches");
    cases
        .into_iter()
        .filter(|case| {
            case.spec.bench.enabled && matches!(case.spec.class, CaseClass::RuntimeSuccess)
        })
        .map(|case| {
            (
                case.name,
                case.program_path
                    .to_str()
                    .expect("program path must be valid utf8")
                    .to_string(),
            )
        })
        .collect()
}

pub fn load_source(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_else(|err| panic!("read {path}: {err}"))
}

pub fn load_program(path: &str) -> Program {
    let source = load_source(path);
    let tokens = lexer::tokenize(&source).unwrap_or_else(|err| panic!("tokenize {path}: {err}"));
    parser::parse_tokens(tokens).unwrap_or_else(|err| panic!("parse {path}: {err}"))
}

pub fn compile_c_binary(source: &str) -> PathBuf {
    let mut dir = std::env::temp_dir();
    dir.push("pyparse-bench");
    fs::create_dir_all(&dir).expect("create bench temp dir");

    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let source_path = dir.join(format!("bench_{nanos}.c"));
    let binary_path = dir.join(format!("bench_{nanos}.bin"));

    fs::write(&source_path, source).expect("write C source");
    let status = Command::new("cc")
        .arg(&source_path)
        .arg("-std=c99")
        .arg("-O2")
        .arg("-o")
        .arg(&binary_path)
        .status()
        .expect("run cc");
    assert!(status.success(), "failed to compile benchmark C source");
    binary_path
}

pub fn detect_python_interpreter() -> Option<String> {
    if let Ok(python) = std::env::var("PYTHON") {
        let status = Command::new(&python).arg("--version").status().ok()?;
        if status.success() {
            return Some(python);
        }
    }

    let status = Command::new("python3").arg("--version").status().ok()?;
    if status.success() {
        return Some("python3".to_string());
    }

    None
}

pub fn run_python_file(interpreter: &str, path: &str) -> String {
    let output = Command::new(interpreter)
        .arg(path)
        .output()
        .unwrap_or_else(|err| panic!("run python file {path}: {err}"));
    assert!(
        output.status.success(),
        "python failed for {path}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("utf8 python stdout")
}

pub fn run_python_startup(interpreter: &str) {
    let status = Command::new(interpreter)
        .arg("-c")
        .arg("pass")
        .status()
        .expect("run python startup");
    assert!(status.success(), "python startup command failed");
}
