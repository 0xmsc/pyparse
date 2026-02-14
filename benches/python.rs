mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};

fn bench_python(c: &mut Criterion) {
    let interpreter = match common::detect_python_interpreter() {
        Some(value) => value,
        None => {
            eprintln!("Skipping Python benchmarks: no PYTHON env or python3 interpreter found.");
            return;
        }
    };

    c.bench_function("python_startup_only", |b| {
        b.iter(|| {
            common::run_python_startup(black_box(&interpreter));
        })
    });

    for (label, path) in common::workloads() {
        c.bench_function(&format!("python_full_runtime_{label}"), |b| {
            b.iter(|| {
                let output = common::run_python_file(black_box(&interpreter), black_box(&path));
                black_box(output);
            })
        });
    }
}

criterion_group!(benches, bench_python);
criterion_main!(benches);
