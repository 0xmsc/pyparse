mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::interpreter::Interpreter;

fn bench_interpreter(c: &mut Criterion) {
    let program = common::load_program();

    c.bench_function("backend_interpreter_total", |b| {
        let mut interpreter = Interpreter::new();
        b.iter(|| {
            let output = interpreter.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });
}

criterion_group!(benches, bench_interpreter);
criterion_main!(benches);
