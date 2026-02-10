mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::Backend;
use pyparse::backend::jit::JIT;

fn bench_jit(c: &mut Criterion) {
    let program = common::load_program();

    c.bench_function("backend_jit_prepare_only", |b| {
        let jit = JIT::new();
        b.iter(|| {
            let prepared = jit.prepare(black_box(&program)).expect("prepare");
            black_box(prepared);
        })
    });

    c.bench_function("backend_jit_execute_prepared", |b| {
        let jit = JIT::new();
        let prepared = jit.prepare(&program).expect("prepare");
        b.iter(|| {
            let output = jit
                .run_prepared(black_box(&prepared))
                .expect("run prepared");
            black_box(output);
        })
    });

    c.bench_function("backend_jit_total", |b| {
        let mut jit = JIT::new();
        b.iter(|| {
            let output = jit.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });
}

criterion_group!(benches, bench_jit);
criterion_main!(benches);
