# ADR-006: Refinement and pattern identity

Status: Accepted
Date: 2026-07-29

## Context

Weisfeiler-Lehman implementations often assign integer colours by hashing a
vertex's current colour and its neighbour colours. Those integers are compact
execution data, but they cannot define semantics: hashes collide, numbering
depends on encounter order, and a query batch may contain signatures absent
from training.

Both WL subtree features and WL optimal assignment depend on stable refinement
identity and parent links.

## Decision

The semantic initial signature of vertex `v` is its node key:

```text
s0(v) = nodeKey(v)
```

At round `r + 1`, the signature is the previous own signature together with the
exact multiset of edge key and previous neighbour signature:

```text
s(r + 1)(v) =
  (s(r)(v), multiset((edgeKey(v, u), s(r)(u)) for u in neighbours(v)))
```

The multiset has a deterministic canonical order for encoding, but neighbour
order is not semantic.

The implementation hash-conses signature DAG nodes. A fast hash chooses a
candidate bucket; exact structural equality resolves collisions. Every
integer colour is an opaque, trace-local index owned by one `RefinementTrace`.
It is invalid without that trace.

Each noninitial signature stores a semantic parent link to the vertex's own
signature at the preceding round. Those links define the WL hierarchy. For WL
optimal assignment, a synthetic root of height zero joins incompatible initial
labels.

Training preparation creates an immutable trace and interner. Query evaluation
uses an immutable overlay or persistent fork. Query-only signatures receive new
local indices and may point to existing ancestors. Concurrent query batches
cannot mutate prepared training state or affect one another.

Persistent signatures require stable codecs for every label and edge key.
Anonymous labels remain executable but can make the semantic expression
ephemeral under ADR-003.

## Rejected alternatives

A 64-bit hash as pattern identity is rejected. Hashes may accelerate lookup but
collisions must not change a kernel value.

Corpus-local colour numbers as public feature keys are rejected because
separate training and query runs can assign the same number to different
signatures.

Mutating the training interner while evaluating queries is rejected because
results would depend on query order and concurrent execution.

Repeated recursive tree materialization is rejected because it duplicates
signature history. The representation is an interned DAG with explicit parent
links.

## Consequences

Vertex reindexing and deliberately adversarial hashes must preserve kernel
values. Query-only subtree features contribute only to query norms. Query-only
WL leaves can still receive optimal-assignment similarity through shared
ancestors. Provenance can name the round, exact signature, parent, hierarchy
node, and multiplicity threshold for every explicit feature.
