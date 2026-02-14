test:
    cargo nextest run --release --test-threads=num-cpus

lint:
    cargo fmt --all
    cargo clippy --fix --allow-dirty --allow-staged --workspace --all-targets -- -D warnings

bench:
    cargo bench

# Run benchmark targets concurrently across available cores with non-interleaved output.
bench-parallel:
    cargo bench --no-run
    parallel 'cargo bench --bench "{}"' ::: frontend interpreter vm jit transpiler python
