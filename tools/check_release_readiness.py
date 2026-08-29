#!/usr/bin/env python3
"""Fail closed until a release receipt records an artifact-only dependency closure."""

from __future__ import annotations

import argparse
from pathlib import Path


def load_properties(path: Path) -> dict[str, str]:
    result: dict[str, str] = {}
    for line_number, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if "=" not in line:
            raise ValueError(f"{path}:{line_number}: expected key=value")
        key, value = line.split("=", 1)
        if not key or key in result:
            raise ValueError(f"{path}:{line_number}: empty or duplicate key {key!r}")
        result[key] = value
    return result


def release_blockers(receipt: dict[str, str]) -> list[str]:
    blockers: list[str] = []
    if receipt.get("release.readiness") != "candidate-ready":
        blockers.append(
            "release.readiness must be candidate-ready "
            f"(observed {receipt.get('release.readiness', '<missing>')})"
        )
    for dependency in ("graph4s", "gale"):
        mode = receipt.get(f"{dependency}.resolution", "<missing>")
        if mode not in {"artifact-candidate", "artifact-release"}:
            blockers.append(
                f"{dependency}.resolution must name an immutable artifact (observed {mode})"
            )
    return blockers


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("receipt", type=Path)
    args = parser.parse_args()
    try:
        receipt = load_properties(args.receipt)
    except (OSError, ValueError) as error:
        parser.error(str(error))
    blockers = release_blockers(receipt)
    if blockers:
        print(f"release preflight blocked for {receipt.get('release.version', '<unknown>')}:")
        for blocker in blockers:
            print(f"- {blocker}")
        return 1
    print(f"release preflight ready for {receipt['release.version']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
