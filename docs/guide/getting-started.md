# Run the executable examples

The fastest trustworthy first success is to compile and run the examples against this checkout.
Grakern does not yet publish release artifacts, and its graph4s and Gale dependencies are fetched
from the immutable revisions recorded in `build.sbt`.

Prerequisites are an sbt installation, a JDK able to run the build, and the native toolchain if you
also run Scala Native tests. Production bytecode targets JDK 17; CI is configured for Temurin 21.

From the repository root, run:

```bash
sbt -batch 'examplesJVM/test' \
  'examplesJVM/runMain grakern.examples.AlphaExampleMain'
```

The main program prints this deterministic summary:

```text
WL train=0.3055555555555556, query=0.1666666666666667, novelNorm=0.8333333333333335, features=23
Shortest path train=2.0, features=2, provenance=2
```

The exact compiled source is
[`AlphaWalkthrough.scala`](../../examples-jvm/src/main/scala/grakern/examples/AlphaWalkthrough.scala).
Its test executes the same methods and checks the documented identity, query, centering,
shortest-path, and provenance behavior. This avoids maintaining a second, almost-equivalent code
sample inside the guide.

For the full repository gate, run:

```bash
sbt -batch scalafmtCheckAll compileAll testAll
```

That command separately compiles and tests the portable modules on JVM, Scala.js, and Scala
Native, then runs the JVM engine, architecture probes, examples, and benchmark-worker tests. A
successful local run is build evidence for that checkout; it is not evidence that artifacts have
been published or that hosted CI has passed the same revision.

Next, follow [the WL workflow](wl-prepared-workflow.md) to understand what the first example does.
