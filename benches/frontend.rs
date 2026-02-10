mod common;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use pyparse::{lexer, parser};

fn bench_frontend(c: &mut Criterion) {
    let source = common::load_source();
    let tokens = lexer::tokenize(&source).expect("tokenize long.py");

    c.bench_function("frontend_tokenize", |b| {
        b.iter(|| {
            let out = lexer::tokenize(black_box(&source)).expect("tokenize");
            black_box(out);
        })
    });

    c.bench_function("frontend_parse_only", |b| {
        b.iter(|| {
            let out = parser::parse_tokens(black_box(tokens.clone())).expect("parse");
            black_box(out);
        })
    });

    c.bench_function("frontend_tokenize_parse", |b| {
        b.iter(|| {
            let tokens = lexer::tokenize(black_box(&source)).expect("tokenize");
            let out = parser::parse_tokens(tokens).expect("parse");
            black_box(out);
        })
    });
}

criterion_group!(benches, bench_frontend);
criterion_main!(benches);
