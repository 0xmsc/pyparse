test:
    cargo nextest run --release

lint:
    cargo fmt --all
    cargo clippy --fix --allow-dirty --allow-staged --workspace --all-targets -- -D warnings
