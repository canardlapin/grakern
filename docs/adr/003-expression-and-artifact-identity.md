# ADR-003: Plan, semantic, and artifact identity

Status: Accepted
Date: 2026-07-29

## Context

The compiler needs to share repeated work within one process and to decide
whether a prepared artifact can be reused later. These decisions need different
forms of identity. A Scala closure does not have a reliable structural
fingerprint, and a sample name does not prove that graph content is unchanged.

## Decision

Grakern uses three identities.

`PlanNodeId` always exists. It identifies one node in one kernel expression
session. Reusing the same `WL.refinement` value gives its consumers the same
plan node even if its label projection is anonymous.

`SemanticFingerprint` is optional. It identifies the kernel denotation to the
degree Grakern can establish from stable descriptors. Every primitive,
parameter, projection, codec, pattern identity, and expression schema version
must be durable before the expression receives one.

`ArtifactFingerprint` is optional. It identifies a prepared result and includes:

- the semantic fingerprint;
- a corpus-content fingerprint;
- ordered sample identity;
- compiler and lowering schema versions;
- numerical policy;
- approximation method and seed;
- relevant engine version.

`SampleKey` supports alignment, diagnostics, and metadata. It is not a
corpus-content fingerprint.

Projections have two constructors:

```scala
Projection.of[A, B](f)
Projection.named[A, B](StableId("domain.field"), version = 1)(f)
```

The first is executable and ephemeral. The second records a user assertion that
the stable identifier and version still describe the closure's meaning.
Grakern does not inspect closure bytecode or claim to verify that assertion.

Fingerprint availability is reported as data:

```scala
enum Fingerprintability:
  case Durable(fingerprint: SemanticFingerprint)
  case Ephemeral(blockers: NonEmptyChain[FingerprintBlocker])
```

A plan report names each blocker and its expression node.

## Rejected alternatives

Hashing closure class names, serialized lambdas, or `toString` output is
rejected because compiler changes and captured state can change them without a
semantic change, or preserve them despite one.

One structural hash for both sharing and persistent caching is rejected.
Session-local object identity is sufficient for sharing but unsafe for durable
reuse.

Sample keys alone are rejected for artifact identity because content can change
under a stable name.

## Consequences

Anonymous closures remain pleasant to use and still share work in one plan.
They disable persistent artifact caching only where their missing descriptor
matters. Durable cache hits require both a durable semantic expression and
durable ordered corpus content.
