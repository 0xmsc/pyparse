test:
    cargo nextest run --release --test-threads=num-cpus

lint:
    cargo fmt --all
    cargo clippy --fix --allow-dirty --allow-staged --workspace --all-targets -- -D warnings
