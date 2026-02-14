mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::{lexer, parser};

fn bench_frontend(c: &mut Criterion) {
    for (label, path) in common::workloads() {
        let source = common::load_source(&path);
        let tokens = lexer::tokenize(&source).expect("tokenize");

        c.bench_function(&format!("frontend_tokenize_{label}"), |b| {
            b.iter(|| {
                let out = lexer::tokenize(black_box(&source)).expect("tokenize");
                black_box(out);
            })
        });

        c.bench_function(&format!("frontend_parse_only_{label}"), |b| {
            b.iter(|| {
                let out = parser::parse_tokens(black_box(tokens.clone())).expect("parse");
                black_box(out);
            })
        });

        c.bench_function(&format!("frontend_tokenize_parse_{label}"), |b| {
            b.iter(|| {
                let tokens = lexer::tokenize(black_box(&source)).expect("tokenize");
                let out = parser::parse_tokens(tokens).expect("parse");
                black_box(out);
            })
        });
    }
}

criterion_group!(benches, bench_frontend);
criterion_main!(benches);
