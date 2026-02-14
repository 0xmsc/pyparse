mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::Backend;
use pyparse::backend::interpreter::Interpreter;
use pyparse::{lexer, parser};

fn bench_interpreter(c: &mut Criterion) {
    for (label, path) in common::workloads() {
        let source = common::load_source(&path);
        let program = common::load_program(&path);
        let interpreter = Interpreter::new();

        c.bench_function(&format!("backend_interpreter_prepare_only_{label}"), |b| {
            b.iter(|| {
                let prepared = interpreter.prepare(black_box(&program)).expect("prepare");
                black_box(prepared);
            })
        });

        c.bench_function(
            &format!("backend_interpreter_run_prepared_only_{label}"),
            |b| {
                let prepared = interpreter.prepare(&program).expect("prepare");
                b.iter(|| {
                    let output = prepared.run().expect("run prepared");
                    black_box(output);
                })
            },
        );

        c.bench_function(
            &format!("backend_interpreter_prepare_plus_run_{label}"),
            |b| {
                b.iter(|| {
                    let output = interpreter
                        .prepare(black_box(&program))
                        .expect("prepare")
                        .run()
                        .expect("run");
                    black_box(output);
                })
            },
        );

        c.bench_function(&format!("backend_interpreter_full_pipeline_{label}"), |b| {
            b.iter(|| {
                let tokens = lexer::tokenize(black_box(&source)).expect("tokenize");
                let parsed_program = parser::parse_tokens(tokens).expect("parse");
                let output = interpreter
                    .prepare(black_box(&parsed_program))
                    .expect("prepare")
                    .run()
                    .expect("run");
                black_box(output);
            })
        });
    }
}

criterion_group!(benches, bench_interpreter);
criterion_main!(benches);
