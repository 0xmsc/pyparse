mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::interpreter::Interpreter;

fn bench_interpreter(c: &mut Criterion) {
    for (label, path) in common::WORKLOADS {
        let program = common::load_program(path);

        c.bench_function(&format!("backend_interpreter_total_{label}"), |b| {
            let interpreter = Interpreter::new();
            b.iter(|| {
                let output = interpreter.run(black_box(&program)).expect("run");
                black_box(output);
            })
        });
    }
}

criterion_group!(benches, bench_interpreter);
criterion_main!(benches);
