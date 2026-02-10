#![allow(dead_code)]
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use pyparse::ast::Program;
use pyparse::{lexer, parser};

pub fn load_source() -> String {
    fs::read_to_string("tests/programs/long.py").expect("read long.py")
}

pub fn load_program() -> Program {
    let source = load_source();
    let tokens = lexer::tokenize(&source).expect("tokenize long.py");
    parser::parse_tokens(tokens).expect("parse long.py")
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
