use std::fs;

use criterion::{Criterion, black_box, criterion_group, criterion_main};

use pyparse::backend::Backend;
use pyparse::{backend, lexer, parser};

fn bench_backends(c: &mut Criterion) {
    let source = fs::read_to_string("tests/programs/long.py").expect("read long.py");
    let tokens = lexer::tokenize(&source);
    let program = parser::parse_tokens(tokens).expect("parse long.py");

    c.bench_function("backend_interpreter", |b| {
        b.iter(|| {
            let mut interpreter = backend::interpreter::Interpreter::new();
            let output = interpreter.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });

    c.bench_function("backend_vm", |b| {
        b.iter(|| {
            let mut vm = backend::vm::VM::new();
            let output = vm.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });

    c.bench_function("backend_transpiler", |b| {
        b.iter(|| {
            let mut transpiler = backend::transpiler::Transpiler;
            let output = transpiler.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });
}

criterion_group!(benches, bench_backends);
criterion_main!(benches);
