mod common;

use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::Backend;
use pyparse::backend::vm::VM;
use pyparse::{lexer, parser};

fn bench_vm(c: &mut Criterion) {
    let mut group = c.benchmark_group("backend_vm");
    for (label, path) in common::workloads() {
        let source = common::load_source(&path);
        let program = common::load_program(&path);
        let vm = VM::new();

        group.bench_function(BenchmarkId::new("prepare_only", &label), |b| {
            b.iter(|| {
                let prepared = vm.prepare(black_box(&program)).expect("prepare");
                black_box(prepared);
            })
        });

        group.bench_function(BenchmarkId::new("run_prepared_only", &label), |b| {
            let prepared = vm.prepare(&program).expect("prepare");
            b.iter(|| {
                let output = prepared.run().expect("run prepared");
                black_box(output);
            })
        });

        group.bench_function(BenchmarkId::new("prepare_plus_run", &label), |b| {
            b.iter(|| {
                let output = vm
                    .prepare(black_box(&program))
                    .expect("prepare")
                    .run()
                    .expect("run");
                black_box(output);
            })
        });

        group.bench_function(BenchmarkId::new("full_pipeline", &label), |b| {
            b.iter(|| {
                let tokens = lexer::tokenize(black_box(&source)).expect("tokenize");
                let parsed_program = parser::parse_tokens(tokens).expect("parse");
                let output = vm
                    .prepare(black_box(&parsed_program))
                    .expect("prepare")
                    .run()
                    .expect("run");
                black_box(output);
            })
        });
    }
    group.finish();
}

criterion_group!(benches, bench_vm);
criterion_main!(benches);
