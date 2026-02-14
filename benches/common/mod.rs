#![allow(dead_code)]
use std::fs;
use std::path::Path;

use pyparse::ast::Program;
use pyparse::{lexer, parser};
use test_support::{
    CaseClass, detect_python_interpreter as detect_python_interpreter_base, load_cases,
    run_python_file as run_python_file_base, run_python_startup as run_python_startup_base,
};

pub fn workloads() -> Vec<(String, String)> {
    let cases = load_cases(Path::new("tests/programs")).expect("load fixture cases for benches");
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

pub fn detect_python_interpreter() -> Option<String> {
    detect_python_interpreter_base()
}

pub fn run_python_file(interpreter: &str, path: &str) -> String {
    run_python_file_base(interpreter, Path::new(path))
        .unwrap_or_else(|err| panic!("run python file {path}: {err}"))
}

pub fn run_python_startup(interpreter: &str) {
    run_python_startup_base(interpreter).expect("run python startup");
}
