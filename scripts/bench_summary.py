#!/usr/bin/env python3
"""Summarize Criterion benchmark results for quick optimization targeting."""

from __future__ import annotations

import argparse
import json
from collections import defaultdict
from pathlib import Path


def format_ns(ns: float) -> str:
    if ns >= 1_000_000_000:
        return f"{ns / 1_000_000_000:.3f}s"
    if ns >= 1_000_000:
        return f"{ns / 1_000_000:.3f}ms"
    if ns >= 1_000:
        return f"{ns / 1_000:.3f}us"
    return f"{ns:.3f}ns"


def backend_from_bench_id(bench_id: str) -> str:
    if bench_id.startswith("backend_"):
        parts = bench_id.split("_", 2)
        if len(parts) >= 2:
            return parts[1]
    if bench_id.startswith("frontend_"):
        return "frontend"
    if bench_id.startswith("python_"):
        return "python"
    return "other"


def load_rows(criterion_dir: Path) -> list[tuple[str, float]]:
    rows: list[tuple[str, float]] = []
    for estimates_path in criterion_dir.rglob("new/estimates.json"):
        bench_dir = estimates_path.parent.parent
        bench_id = bench_dir.name
        data = json.loads(estimates_path.read_text())
        median_ns = float(data["median"]["point_estimate"])
        rows.append((bench_id, median_ns))
    return rows


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--criterion-dir", default="target/criterion")
    parser.add_argument("--top", type=int, default=12)
    args = parser.parse_args()

    criterion_dir = Path(args.criterion_dir)
    if not criterion_dir.exists():
        print(f"No Criterion output found at {criterion_dir}")
        print("Run `just bench-parallel` first.")
        return 1

    rows = load_rows(criterion_dir)
    if not rows:
        print(f"No benchmark estimates found under {criterion_dir}")
        print("Run `just bench-parallel` first.")
        return 1

    backend_totals: dict[str, float] = defaultdict(float)
    backend_counts: dict[str, int] = defaultdict(int)
    for bench_id, median_ns in rows:
        backend = backend_from_bench_id(bench_id)
        backend_totals[backend] += median_ns
        backend_counts[backend] += 1

    print("Backend totals (sum of median per benchmark):")
    for backend, total_ns in sorted(
        backend_totals.items(), key=lambda item: item[1], reverse=True
    ):
        count = backend_counts[backend]
        avg_ns = total_ns / count
        print(
            f"  {backend:<12} total={format_ns(total_ns):>10} "
            f"avg={format_ns(avg_ns):>10} n={count}"
        )

    print("")
    print(f"Top {args.top} slowest benchmark functions (median):")
    for bench_id, median_ns in sorted(rows, key=lambda item: item[1], reverse=True)[: args.top]:
        print(f"  {format_ns(median_ns):>10}  {bench_id}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
