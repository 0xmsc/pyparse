#!/usr/bin/env python3
"""Summarize Criterion benchmark results by backend/workload/phase."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def format_ns(ns: float) -> str:
    if ns >= 1_000_000_000:
        return f"{ns / 1_000_000_000:.3f}s"
    if ns >= 1_000_000:
        return f"{ns / 1_000_000:.3f}ms"
    if ns >= 1_000:
        return f"{ns / 1_000:.3f}us"
    return f"{ns:.3f}ns"


def load_rows(criterion_dir: Path) -> list[tuple[str, str | None, str | None, float]]:
    rows: list[tuple[str, str | None, str | None, float]] = []
    for estimates_path in criterion_dir.rglob("new/estimates.json"):
        benchmark_path = estimates_path.parent / "benchmark.json"
        if not benchmark_path.exists():
            continue

        data = json.loads(estimates_path.read_text())
        benchmark = json.loads(benchmark_path.read_text())
        group_id = benchmark.get("group_id")
        if not isinstance(group_id, str):
            continue
        function_id = benchmark.get("function_id")
        if function_id is not None and not isinstance(function_id, str):
            continue
        value_str = benchmark.get("value_str")
        if value_str is not None and not isinstance(value_str, str):
            continue
        median_ns = float(data["median"]["point_estimate"])
        rows.append((group_id, function_id, value_str, median_ns))
    return rows


def parse_rows(
    rows: list[tuple[str, str | None, str | None, float]],
) -> tuple[dict[tuple[str, str], dict[str, float]], float | None]:
    table: dict[tuple[str, str], dict[str, float]] = {}
    python_startup_ns: float | None = None

    for group_id, function_id, value_str, median_ns in rows:
        if group_id.startswith("backend_") and function_id and value_str:
            backend = group_id.removeprefix("backend_")
            phase = function_id
            workload = value_str
            key = (backend, workload)
            if key not in table:
                table[key] = {}

            if phase == "prepare_only":
                table[key]["prepare"] = median_ns
            elif phase == "run_prepared_only":
                table[key]["run"] = median_ns
            elif phase == "prepare_plus_run":
                table[key]["total"] = median_ns
            continue

        if group_id == "python" and function_id == "startup_only":
            python_startup_ns = median_ns
            continue

        if group_id == "python" and function_id == "full_runtime" and value_str:
            workload = value_str
            key = ("python", workload)
            if key not in table:
                table[key] = {}
            table[key]["total"] = median_ns

    if python_startup_ns is not None:
        for (backend, _workload), phases in table.items():
            if backend != "python":
                continue
            phases["prepare"] = python_startup_ns
            if "total" in phases:
                phases["run"] = max(phases["total"] - python_startup_ns, 0.0)

    for phases in table.values():
        if "total" not in phases and "prepare" in phases and "run" in phases:
            phases["total"] = phases["prepare"] + phases["run"]

    return table, python_startup_ns


def format_cell(value: float | None) -> str:
    return "-" if value is None else format_ns(value)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--criterion-dir", default="target/criterion")
    parser.add_argument("--workload", help="Filter to one workload label (e.g. bench_gcd).")
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

    table, python_startup_ns = parse_rows(rows)
    entries = []
    for (backend, workload), phases in table.items():
        if args.workload and workload != args.workload:
            continue
        has_complete_timing = ("total" in phases) or (
            "prepare" in phases and "run" in phases
        )
        if not has_complete_timing:
            continue
        entries.append((backend, workload, phases))

    if not entries:
        if args.workload:
            print(f"No matching benchmark rows for workload `{args.workload}`.")
        else:
            print("No matching backend benchmark rows found.")
            print("Run benches after migrating to Criterion BenchmarkId layout.")
        return 1

    entries.sort(key=lambda item: (item[0], item[1]))

    print("Backend/workload timing (median):")
    header = f"{'backend':<12} {'workload':<16} {'prepare':>12} {'run':>12} {'total':>12}"
    print(header)
    print("-" * len(header))
    previous_backend: str | None = None
    for backend, workload, phases in entries:
        if previous_backend is not None and backend != previous_backend:
            print("----")
        print(
            f"{backend:<12} {workload:<16} "
            f"{format_cell(phases.get('prepare')):>12} "
            f"{format_cell(phases.get('run')):>12} "
            f"{format_cell(phases.get('total')):>12}"
        )
        previous_backend = backend

    if python_startup_ns is not None:
        print("")
        print(
            "Note: CPython `prepare` uses `python_startup_only`; "
            "`run` is computed as `full_runtime - startup`."
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
