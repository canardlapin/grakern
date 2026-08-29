# Use Grakern's alpha surface

This guide is for Scala users evaluating exact graph kernels when the meaning of a kernel, the
identity of its inputs, and the numerical realization all need to remain inspectable. Grakern is
still source-only `0.1-development` software: the guide describes the current checkout, not published Maven
coordinates or a compatibility promise.

Three ideas organize the public surface:

1. A graph sample keeps graph-local vertex identity separate from node and edge values.
2. A kernel recipe states the mathematical computation; a JVM plan prepares concrete features,
   matrices, operators, and reports from that recipe.
3. Durable fingerprints and provenance are explicit capabilities. When their prerequisites are
   absent, Grakern reports that fact instead of inventing an identity or explanation.

## Reader path

1. [Run the executable examples](getting-started.md) to establish a working checkout.
2. [Prepare and query a WL kernel](wl-prepared-workflow.md) to learn the ordinary training and
   query workflow.
3. [Use shortest-path features and provenance](shortest-path-workflow.md) when path structure is
   the desired decomposition.
4. [Interpret identity, plans, and failures](identity-plans-and-failures.md) before persisting or
   comparing results.

The architecture decisions in [`docs/adr`](../adr/) explain why these boundaries exist. They are
reference material; newcomers do not need to read them before completing the walkthrough.

## Current capability boundary

The portable `core`, `graph4s-adapter`, `standard`, and `laws` modules compile on the JVM,
Scala.js, and Scala Native. The prepared compiler and Gale-backed numerical views are JVM-only.
The current alpha surface includes exact WL subtree/OA composition, categorical and soft
shortest-path kernels, histogram features, query-aligned WL cross values, and reference-bound
centering.

It does not yet promise binary compatibility, published artifacts, a public lowering SPI,
histogram query alignment, directed shortest-path semantics, approximation, or commute-time
kernels.
