#!/usr/bin/env python3
"""Require a versioned receipt whenever a committed performance baseline moves."""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path


BASELINE_PREFIX = "benchmarks/court/baselines/"
REPORT_PREFIX = "benchmarks/court/reports/"


def changed_paths(repository: Path, base: str, head: str) -> set[str]:
    completed = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=ACMR", f"{base}...{head}"],
        cwd=repository,
        check=True,
        capture_output=True,
        text=True,
    )
    return {line for line in completed.stdout.splitlines() if line}


def missing_receipt(paths: set[str]) -> bool:
    baseline_changed = any(path.startswith(BASELINE_PREFIX) for path in paths)
    report_changed = any(
        path.startswith(REPORT_PREFIX) and path.endswith(".md") for path in paths
    )
    return baseline_changed and not report_changed


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", required=True)
    parser.add_argument("--head", default="HEAD")
    parser.add_argument("--repository", type=Path, default=Path.cwd())
    arguments = parser.parse_args()

    paths = changed_paths(arguments.repository, arguments.base, arguments.head)
    if missing_receipt(paths):
        print(
            "performance baseline changed without a versioned report under "
            "benchmarks/court/reports"
        )
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
