use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use criterion::{Criterion, black_box, criterion_group, criterion_main};

use pyparse::backend::Backend;
use pyparse::{backend, lexer, parser};

fn compile_c_binary(source: &str) -> PathBuf {
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

fn bench_backends(c: &mut Criterion) {
    let source = fs::read_to_string("tests/programs/long.py").expect("read long.py");
    let tokens = lexer::tokenize(&source).expect("tokenize long.py");
    let program = parser::parse_tokens(tokens.clone()).expect("parse long.py");

    c.bench_function("frontend_tokenize", |b| {
        b.iter(|| {
            let out = lexer::tokenize(black_box(&source)).expect("tokenize");
            black_box(out);
        })
    });

    c.bench_function("frontend_parse_only", |b| {
        b.iter(|| {
            let out = parser::parse_tokens(black_box(tokens.clone())).expect("parse");
            black_box(out);
        })
    });

    c.bench_function("frontend_tokenize_parse", |b| {
        b.iter(|| {
            let tokens = lexer::tokenize(black_box(&source)).expect("tokenize");
            let out = parser::parse_tokens(tokens).expect("parse");
            black_box(out);
        })
    });

    c.bench_function("backend_interpreter_total", |b| {
        let mut interpreter = backend::interpreter::Interpreter::new();
        b.iter(|| {
            let output = interpreter.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });

    c.bench_function("backend_vm_compile_only", |b| {
        b.iter(|| {
            let compiled = backend::bytecode::compile(black_box(&program)).expect("compile");
            black_box(compiled);
        })
    });

    c.bench_function("backend_vm_total", |b| {
        let mut vm = backend::vm::VM::new();
        b.iter(|| {
            let output = vm.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });

    c.bench_function("backend_jit_total", |b| {
        let mut jit = backend::jit::JIT::new();
        b.iter(|| {
            let output = jit.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });

    c.bench_function("backend_transpiler_codegen_only", |b| {
        let transpiler = backend::transpiler::Transpiler;
        b.iter(|| {
            let source = transpiler
                .transpile(black_box(&program))
                .expect("transpile");
            black_box(source);
        })
    });

    c.bench_function("backend_transpiler_total", |b| {
        let mut transpiler = backend::transpiler::Transpiler;
        b.iter(|| {
            let output = transpiler.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });

    let transpiler = backend::transpiler::Transpiler;
    let c_source = transpiler.transpile(&program).expect("transpile C source");
    let c_binary = compile_c_binary(&c_source);
    c.bench_function("backend_transpiler_precompiled_exec_only", |b| {
        b.iter(|| {
            let output = Command::new(black_box(&c_binary))
                .output()
                .expect("run precompiled C");
            assert!(output.status.success(), "precompiled C failed");
            let output = String::from_utf8(output.stdout).expect("utf8 output");
            black_box(output);
        })
    });
}

criterion_group!(benches, bench_backends);
criterion_main!(benches);
