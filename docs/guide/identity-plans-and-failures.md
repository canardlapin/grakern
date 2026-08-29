# Interpret identity, plans, and failures

Grakern separates a value that can execute now from an artifact that can be identified durably.
Anonymous functions are valid for exploratory execution, but their meaning cannot be recovered
from a serialized artifact.

## Make semantic identity durable

A durable WL identity requires named, versioned node and edge projections plus named, versioned
stable codecs for their keys. The codec is a user assertion: its encoding must remain stable and
semantically injective for the values used.

```scala
val nodeProjection =
  Projection.named[Atom, String](nodeProjectionId, StableVersion.initial)(_.element)

val nodeCodec =
  StableCodec.named[String](nodeCodecId, StableVersion.initial)(identity)
```

An anonymous projection produces `Fingerprintability.Ephemeral` with an
`AnonymousProjection` blocker. A missing codec produces a `MissingCodec` blocker. Portable WL
feature dictionaries follow the same boundary: durable coordinates can be serialized and restored;
ephemeral coordinates return the typed `WLFeatureDictionaryError.EphemeralCoordinate` refusal.

Artifact identity adds the ordered sample keys, a caller-supplied corpus-content fingerprint,
compiler/lowering schemas, and numerical policy. A durable semantic fingerprint without a corpus
fingerprint therefore remains `ArtifactFingerprintability.Ephemeral` with
`MissingCorpusContentFingerprint`.

## Read the plan report before interpreting views

`planReport.explain` is a deterministic text explanation of the plan graph and realized views. For
WL it includes refinement sharing, feature shape and density, Gram and operator realizations,
normalization, query alignment, memory/cost estimates, execution counts, and both identity states.
Shortest-path reports include the APSP algorithm, lowering, capabilities, search counts, and any
dual-realization difference.

The report is evidence about this local preparation. It is not a performance benchmark, a
publication receipt, or proof that a different checkout chose the same realization.

## Distinguish metadata from provenance

WL feature-matrix columns carry typed subtree or OA metadata. The current public `WLCompiledKernel`
does not expose graph-local representative occurrences for those columns, despite retaining the
training traces internally; treat WL source-level provenance as an alpha API gap. Categorical
shortest-path and histogram feature preparations do expose graph-local provenance and per-cell
contributions.

This distinction matters when a downstream workflow needs an audit trail. Typed metadata says what
a coordinate denotes. Provenance additionally says which source objects emitted or supported it.

## Recover from common failures

| Failure | What it means | Correction |
|---|---|---|
| `GraphSampleEdgeListError` | The input cannot be represented as a simple undirected graph4s graph | Correct the input, use `NeighbourhoodSample`, or reify relations |
| `WLRefinementError.NegativeRounds` | Refinement depth is outside the domain | Supply zero or more rounds |
| `Fingerprintability.Ephemeral` | Execution is allowed, durable semantic identity is not | Name/version projections and provide stable codecs |
| `MissingCorpusContentFingerprint` | The plan lacks durable corpus content identity | Hash the canonical corpus outside Grakern and pass the resulting fingerprint |
| `WLQueryCompileError` | Query refinement, normalization, or sparse materialization failed | Inspect the typed nested error; training remains immutable |
| `PreparedCenteringError` | The reference is empty or a cross view has the wrong identity/shape | Reuse the exact prepared reference and its ordered columns |
| shortest-path capability refusal | The selected pairwise lowering deliberately has no feature/provenance view | Select `ExactFeatures` or `ValidateBoth` |

For mathematical ownership and longer rationale, continue to
[`docs/architecture.md`](../architecture.md) and the accepted ADRs.
