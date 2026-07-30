# Performance court runner

`run_performance_court.py` coordinates JVM, Python, and Julia benchmark
workers. It does not implement graph kernels. Each worker receives one JSON
job for one process fork and returns measurements through the same JSON
protocol.

The runner enforces [ADR 008](../../docs/adr/008-performance-court.md). It
fingerprints sources, dependency locks, runtimes, the workload manifest, and
the runner itself before accepting results. It also checks the host before and
before and after each fork, captures logs, validates semantic output, retains
raw samples, and classifies each warm Grakern row against the fastest admitted
comparator.

## Run the tests

The runner depends on `psutil` and `jsonschema`. The repository does not add
them to the Scala build.

```bash
uv run \
  --with psutil \
  --with jsonschema \
  python -m unittest discover \
  -s benchmarks/court/tests \
  -v
```

## Plan a court

A plan conforming to `court-plan.schema.json` declares:

- whether it is a complete `championship` plan or a
  `diagnostic_subset`;
- source repositories and the files that lock their dependencies;
- host-admission thresholds;
- the fixed classification thresholds from ADR 008;
- the deterministic execution-order seed;
- one command for each implementation, row, boundary, and cold or warm mode.

A championship plan must cover every Tier-1 row at both warm boundaries for
Grakern and every eligible comparator. It must also contain the cold public
end-to-end jobs. A diagnostic subset can exercise fewer jobs, but the runner
accepts it only with `--diagnostic` and its receipts remain inadmissible.

The command must contain `{job_json}` and `{worker_result_json}` placeholders.
The runner replaces them with absolute paths. It invokes each command in a
fresh process for every required fork, directly and without a shell. Within
each row, boundary, and mode, it rotates implementation order by fork. This
prevents one implementation from always running first on a cooler or less
fragmented process state.

For example, worker commands may have these shapes:

```json
["java", "-jar", "grakern-benchmark-worker.jar",
 "--job", "{job_json}", "--output", "{worker_result_json}"]
```

```json
["uv", "run", "--python", "3.11",
 "tools/grakel/benchmark_worker.py",
 "--job", "{job_json}", "--output", "{worker_result_json}"]
```

```json
["julia", "--project=tools/graphkernels-jl",
 "tools/graphkernels-jl/benchmark_worker.jl",
 "--job", "{job_json}", "--output", "{worker_result_json}"]
```

Each job also declares a `version_command`. The runner executes that command
before and after the worker. A changed runtime fingerprint invalidates the
job. External jobs must describe their semantic translation, including an
explicit identity translation when no compatibility adjustment is needed.

## Worker responsibilities

A worker result must conform to `worker-result.schema.json`. The worker owns:

- one cold or warm process fork;
- the declared warmup, sample, and batch policy for that fork;
- language-specific phase and allocation measurements;
- materialization of the requested matrix;
- comparison with the independent expected output;
- canonical output and sample-order checksums;
- proof checks specific to the kernel.

The runner owns process independence and cross-implementation ordering. It
verifies each fork's sample dimensions, timed-work accounting, median, finite
JSON numbers, job identity, language, mode, thread-control environment, phase
timings, and semantic result. It then aggregates the forks, checks the minimum
fork and timed-work policy, and records the execution-order digest. Grakern
workers must report allocation measurements. For a floating-tolerance row,
the worker must return maximum absolute and relative errors as well as both
tolerances.

For cold forks, the receipt uses the runner-observed duration from process
launch through successful exit. This includes runtime startup and worker
verification overhead. Cold latency is reported separately and never decides
the Tier-1 winner.

The worker's expected checksum is not authoritative. Every admitted Tier-1
row carries an independently materialized `expected_output` contract with the
shape, sample-order digest, comparison policy, and expected-output digest. The
runner requires the worker result to match that contract before it considers
the worker's observed output.

A worker may apply a declared external compatibility translation after the
timed operation and before semantic hashing. It must not include the
translation in the timed implementation or change the target Grakern
denotation.

## Preflight

Write court output outside every source checkout. This prevents receipts from
making a source tree dirty during a run.

The runner also takes a host-local lock for the current operating-system user.
Two Grakern courts cannot run concurrently under different output roots and
silently contaminate each other's timings.

```bash
uv run --with psutil --with jsonschema \
  python benchmarks/court/run_performance_court.py \
  preflight benchmarks/court/active-plan.json \
  /tmp/grakern-performance-court
```

Preflight writes `preflight.json`. If a required source or host fact fails, it
returns a nonzero status and names the failed checks.

Use `--diagnostic` on a developer machine when you need to test a worker
without making a performance claim. Diagnostic receipts remain visibly
inadmissible.

## Run and resume

```bash
uv run --with psutil --with jsonschema \
  python benchmarks/court/run_performance_court.py \
  run benchmarks/court/active-plan.json \
  /tmp/grakern-performance-court
```

The runner checkpoints `court-state.json` after every complete race group.
Resume only with the same plan, runner files, source fingerprints, and
completed draft hashes:

```bash
uv run --with psutil --with jsonschema \
  python benchmarks/court/run_performance_court.py \
  run benchmarks/court/active-plan.json \
  /tmp/grakern-performance-court \
  --resume
```

Resume fails if a completed artifact or source has changed. It never mixes
samples from two revisions.

## Output

Each job directory contains one subdirectory per fork and an aggregate
directory. The aggregate manifest records every fork's process, position,
declared order, input, result, stdout, stderr, and digest. The job directory
also contains the proof log and receipt draft. Final receipts live under
`receipts/` and conform to `receipt.schema.json`.

The court state distinguishes completed, diagnostic, and blocked runs. A
summary or league table must be derived from the receipts. It cannot replace
the raw samples and proof artifacts that support each row.
