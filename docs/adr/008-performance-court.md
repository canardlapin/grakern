# ADR 008: Correctness-locked performance court

## Status

Accepted before the E7 performance campaign.

## Purpose

This ADR defines which benchmark results Grakern may use for performance
claims. It covers semantic admission, measured boundaries, source and host
admission, statistical classification, memory and scaling checks, and
comparator defects.

A fast wrong answer is not a benchmark result. A correct result from a dirty
checkout or an active host is useful diagnostic evidence, but it is not
championship evidence.

## Semantic admission

Each benchmark row names one kernel denotation, corpus order, numerical policy,
and requested output. A runner must establish all of the following before it
records timing:

1. Safe Grakern constructors carry the mathematical claim.
2. The optimized result agrees with an independent Grakern reference on the
   row's reference fixture.
3. Required metamorphic checks pass, including vertex reindexing and
   collision-independence where applicable.
4. Every external comparator has an explicit semantic translation. The
   translation may account for a documented occurrence or iteration
   convention, but it may not discard information or change the kernel.
5. The output shape, sample order, and semantic checksum agree.

The court uses exact row-major IEEE-754 checksums for the unnormalized
integer-valued Tier-1 kernels. It canonicalizes negative zero to positive zero
before hashing. A row that requires floating comparison instead records
absolute and relative tolerances, compares every element, and hashes the
canonical expected output separately. NaN and infinity always fail admission.

The evidence ladder is:

1. mathematical constructor and denotation;
2. independent scalar or combinatorial reference;
3. optimized/reference differential and metamorphic tests;
4. external compatibility fixtures after source-level alignment.

An external library never overrides the first three levels.

## Comparator admission and defects

The first court admits:

- GraKeL distribution 0.1.10 in the pinned Python environment;
- GraphKernels.jl 0.2.0 at source revision
  `f04797de0b86a39172b6adbcfcee35ec4b39f91b`.

The row manifest states which comparator is semantically eligible. For
example, the pinned GraphKernels.jl WL implementation is not eligible for
general non-regular WL rows, and its shortest-path result requires the
documented ordered-pair and additive-endpoint translation.

When a mismatch appears, the runner stops that row before timing. The
investigation must:

1. reduce the mismatch to a small deterministic fixture;
2. compare both implementations with the independent reference;
3. classify the cause as a Grakern defect, comparator defect, translation
   defect, numerical-policy difference, or unresolved mismatch;
4. record the comparator revision and the relevant source location;
5. add a permanent regression test.

A confirmed comparator defect also requires an upstream-ready report containing
the reproducer, expected and observed values, environment, and source
attribution. Filing the report is a separate external action.

Excluding a defective comparator does not create a Grakern win. A Tier-1 row
without at least one admitted correct external comparator is `uncontested` and
cannot pass the performance gate.

## Measured boundaries

Every Tier-1 operation is measured at both boundaries below.

### Public end to end

The timer starts with a neutral immutable corpus representation already in
memory. It includes:

- construction of the library's public graph objects and fields;
- adaptation and validation;
- Grakern packing or the comparator's corresponding conversion;
- refinement or APSP;
- feature extraction or pairwise evaluation;
- complete materialization of the requested Gram or cross matrix.

File download, disk I/O, fixture decoding, output checksum calculation, and
benchmark reporting remain outside the timer. The neutral corpus is identical
for every implementation.

### Prepared topology

The timer starts after the runner has built the library-native graph topology
and attached the raw node and edge fields. It still includes every
kernel-specific conversion, projection, packing, refinement, APSP, feature, and
matrix cost.

This boundary shows whether graph construction owns a loss without allowing a
runner to hide kernel preprocessing in setup.

No timed iteration may reuse a prepared kernel, feature dictionary, APSP trace,
Gram matrix, or other result that the measured public operation would have to
create. The operator rows, when added, will define their own repeated-apply
boundary instead of reusing this one.

## Cold and warm execution

Cold and warm measurements are different products and are never combined.

- A cold receipt starts a fresh process and includes runtime startup, imports,
  class loading, JIT or Julia compilation, and one complete public
  end-to-end operation.
- A warm receipt starts a fresh process, performs the declared warmup, then
  times complete operations from a reset operation state. Runtime code and
  filesystem caches may be warm; kernel artifacts may not be reused.

Cold startup is reported for every implementation but does not decide the
Tier-1 speed winner. Warm wall-clock time decides the speed classification.

The initial sampling contract is:

- at least 10 independent process forks;
- at least 10 timed operations per fork;
- at least 10 untimed warmup operations per fork;
- a calibrated inner batch only when one operation is shorter than 100 ms;
- at least 30 seconds of timed work for each implementation and row.

Calibration runs in a disposable process. The recorded process uses the fixed
batch count chosen by calibration. Implementations run in a deterministic
rotating order across forks. The receipt stores every raw duration and the
fork and batch membership.

The court may reduce the sample count for a row whose single operation exceeds
two seconds, but it still requires at least five forks, five samples per fork,
and 30 seconds of timed work. The receipt must record that expensive-row
policy.

## Source admission

Before and after each phase, the runner records:

- Grakern commit, tree digest, and clean-worktree status;
- exact Graph4s and Gale source revisions;
- comparator source or distribution revision;
- dependency-lock digests;
- runner source digest;
- workload manifest and corpus content digests;
- Java, Scala, Python, NumPy, Julia, compiler, and operating-system versions;
- all environment variables that can change threading or numerical behavior.

Any source or lock change invalidates the phase. A dirty checkout is
inadmissible. Local sibling worktrees are never implicit benchmark inputs:
Graph4s and Gale must be exact pinned sources recorded in the receipt.

## Host admission

The championship court runs on a dedicated, AC-powered host with performance
mode enabled. Before and after each phase, the runner must establish:

- no other benchmark or build process is active;
- non-court CPU use remains below 5 percent for 30 seconds;
- no swap growth or operating-system memory-pressure warning occurs;
- free memory exceeds twice the row's prior peak RSS plus 2 GiB;
- the host is not thermally or power throttled;
- a fixed compute calibration has coefficient of variation at most 2 percent;
- the before/after calibration medians differ by at most 2 percent.

If the platform cannot establish one of these facts, the receipt records
`blocked_host` and the court makes no speed claim. Diagnostic runs on a busy
developer machine must use `diagnostic_only`.

## Threading

The Tier-1 championship uses one application thread and one BLAS thread.
Single-thread admission makes the work performed by each runtime explicit and
available on every comparator.

Parallel scaling is a separate required court at 1, 2, 4, and 8 application
threads, capped at the host's physical-core count. It reports speedup,
efficiency, output checksum, peak RSS, and deterministic reproducibility. A
parallel result may not repair a losing single-thread Tier-1 row.

## Speed classification

For each row and boundary, Grakern races the fastest admitted correct external
comparator. Results are classified independently; no geometric mean or total
score can hide a losing row.

Let `R` be the ratio of Grakern warm wall time to comparator warm wall time.
The court uses a cluster bootstrap over process forks and retains the
within-fork samples. A `win` requires both:

- the median ratio is at most `0.90`; and
- the upper endpoint of the 99 percent confidence interval is at most `0.95`.

A result is a `loss` when the median ratio is at least `1.00`. Every other
correct result is a `tie`. A tie is not a win.

The receipt stores the estimator, bootstrap seed, resample count, medians,
confidence interval, and raw samples. The initial court uses at least 20,000
bootstrap resamples.

## Memory, capacity, and scaling

Peak resident memory is measured out of process. The receipt records absolute
peak RSS and the increase above the runtime/import baseline.

A memory difference is material when Grakern exceeds the lowest admitted
correct comparator by both 10 percent and 64 MiB. A material memory loss keeps
the row open even when its timing result is a win.

Grakern must also:

- complete the largest admitted corpus within the row's memory and time
  budget;
- avoid an earlier capacity failure than every correct comparator;
- show no worse empirical scaling exponent by more than 0.10 on the declared
  size ladder;
- retain exact checksums at every thread count and scale.

Allocation metrics are mandatory for Grakern and recorded where the comparator
runtime exposes a comparable definition. Missing comparator allocation data
must be marked unavailable; it must not be reported as zero.

## Tier-1 championship

`benchmarks/court/tier1.json` contains the closed row list. It covers WL
subtree Gram, WL optimal-assignment Gram, categorical shortest-path Gram, and
unnormalized WL subtree cross on sparse, dense, multiplicity-heavy,
GraphKernels.jl-aligned, and MUTAG corpora.

The workload task must materialize every corpus, record its content digest, and
produce the independent expected outputs before the first baseline. Changing a
Tier-1 row after baseline collection requires a new ADR revision and invalidates
the affected baseline.

The performance gate passes only when:

- every Tier-1 row and both boundaries are semantically admitted;
- every row is a speed `win` against its fastest correct comparator;
- no material memory, capacity, or scaling loss remains;
- all raw receipts reproduce from immutable sources on an admitted host;
- every comparator defect follows the defect protocol.

Unsupported, uncontested, tied, noisy, or aggregated results cannot pass the
gate.

## Receipt

Every run writes one document conforming to
`benchmarks/court/receipt.schema.json`. Raw samples and proof logs are immutable
artifacts referenced by SHA-256. A summary table is a view over receipts, not a
replacement for them.

The harness must fail closed when a required field is unavailable. It may emit
a diagnostic or blocked receipt, but it may not silently downgrade a missing
checksum, source fingerprint, host check, or comparator.
