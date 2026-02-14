mod common;

use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::Backend;
use pyparse::backend::jit::JIT;
use pyparse::{lexer, parser};

fn bench_jit(c: &mut Criterion) {
    let mut group = c.benchmark_group("backend_jit");
    for (label, path) in common::workloads() {
        let source = common::load_source(&path);
        let program = common::load_program(&path);
        let jit = JIT::new();

        group.bench_function(BenchmarkId::new("prepare_only", &label), |b| {
            b.iter(|| {
                let prepared = Backend::prepare(&jit, black_box(&program)).expect("prepare");
                black_box(prepared);
            })
        });

        group.bench_function(BenchmarkId::new("run_prepared_only", &label), |b| {
            let prepared = Backend::prepare(&jit, &program).expect("prepare");
            b.iter(|| {
                let output = prepared.run().expect("run prepared");
                black_box(output);
            })
        });

        group.bench_function(BenchmarkId::new("prepare_plus_run", &label), |b| {
            b.iter(|| {
                let prepared = Backend::prepare(&jit, black_box(&program)).expect("prepare");
                let output = prepared.run().expect("run");
                black_box(output);
            })
        });

        group.bench_function(BenchmarkId::new("full_pipeline", &label), |b| {
            b.iter(|| {
                let tokens = lexer::tokenize(black_box(&source)).expect("tokenize");
                let parsed_program = parser::parse_tokens(tokens).expect("parse");
                let prepared = Backend::prepare(&jit, black_box(&parsed_program)).expect("prepare");
                let output = prepared.run().expect("run");
                black_box(output);
            })
        });
    }
    group.finish();
}

criterion_group!(benches, bench_jit);
criterion_main!(benches);
