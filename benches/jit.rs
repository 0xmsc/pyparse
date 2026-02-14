mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::Backend;
use pyparse::backend::jit::JIT;
use pyparse::{lexer, parser};

fn bench_jit(c: &mut Criterion) {
    for (label, path) in common::workloads() {
        let source = common::load_source(&path);
        let program = common::load_program(&path);
        let jit = JIT::new();

        c.bench_function(&format!("backend_jit_prepare_only_{label}"), |b| {
            b.iter(|| {
                let prepared = Backend::prepare(&jit, black_box(&program)).expect("prepare");
                black_box(prepared);
            })
        });

        c.bench_function(&format!("backend_jit_run_prepared_only_{label}"), |b| {
            let prepared = Backend::prepare(&jit, &program).expect("prepare");
            b.iter(|| {
                let output = prepared.run().expect("run prepared");
                black_box(output);
            })
        });

        c.bench_function(&format!("backend_jit_prepare_plus_run_{label}"), |b| {
            b.iter(|| {
                let prepared = Backend::prepare(&jit, black_box(&program)).expect("prepare");
                let output = prepared.run().expect("run");
                black_box(output);
            })
        });

        c.bench_function(&format!("backend_jit_full_pipeline_{label}"), |b| {
            b.iter(|| {
                let tokens = lexer::tokenize(black_box(&source)).expect("tokenize");
                let parsed_program = parser::parse_tokens(tokens).expect("parse");
                let prepared = Backend::prepare(&jit, black_box(&parsed_program)).expect("prepare");
                let output = prepared.run().expect("run");
                black_box(output);
            })
        });
    }
}

criterion_group!(benches, bench_jit);
criterion_main!(benches);
