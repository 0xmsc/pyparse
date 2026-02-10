mod common;

use std::process::Command;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::Backend;
use pyparse::backend::transpiler::Transpiler;

fn bench_transpiler(c: &mut Criterion) {
    let program = common::load_program();

    c.bench_function("backend_transpiler_codegen_only", |b| {
        let transpiler = Transpiler;
        b.iter(|| {
            let source = transpiler
                .transpile(black_box(&program))
                .expect("transpile");
            black_box(source);
        })
    });

    c.bench_function("backend_transpiler_total", |b| {
        let mut transpiler = Transpiler;
        b.iter(|| {
            let output = transpiler.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });

    let transpiler = Transpiler;
    let c_source = transpiler.transpile(&program).expect("transpile C source");
    let c_binary = common::compile_c_binary(&c_source);
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

criterion_group!(benches, bench_transpiler);
criterion_main!(benches);
