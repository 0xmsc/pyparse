mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::backend::Backend;
use pyparse::backend::{bytecode, vm};

fn bench_vm(c: &mut Criterion) {
    let program = common::load_program();

    c.bench_function("backend_vm_compile_only", |b| {
        b.iter(|| {
            let compiled = bytecode::compile(black_box(&program)).expect("compile");
            black_box(compiled);
        })
    });

    c.bench_function("backend_vm_execute_prepared", |b| {
        let compiled = bytecode::compile(&program).expect("compile");
        let mut vm = vm::VM::new();
        b.iter(|| {
            let output = vm.run_compiled(black_box(&compiled)).expect("run compiled");
            black_box(output);
        })
    });

    c.bench_function("backend_vm_total", |b| {
        let mut vm = vm::VM::new();
        b.iter(|| {
            let output = vm.run(black_box(&program)).expect("run");
            black_box(output);
        })
    });
}

criterion_group!(benches, bench_vm);
criterion_main!(benches);
