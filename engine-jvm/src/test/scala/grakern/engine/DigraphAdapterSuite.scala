package grakern.engine

import cats.kernel.{Eq, Hash}
import grakern.core.*
import grakern.standard.wl.*
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

/** The second (test-scope) adapter: plain directed / multi-edge neighbourhoods built with
  * `NeighbourhoodSample`, run end to end through WL refinement and the compiled kernel.
  */
final class DigraphAdapterSuite extends ScalaCheckSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

  private def refinement(rounds: Int = 2) =
    WLRefinement
      .create(
        rounds,
        Projection.of[String, String](identity),
        Projection.of[String, String](identity),
        WLCodecs[String, String](None, None)
      )
      .toOption
      .get

  private def directed(key: String, labels: Map[Int, String], arcs: Vector[(Int, Int, String)]) =
    NeighbourhoodSample
      .directed(SampleKey(key), labels.keys.toVector.sorted, labels, arcs)
      .toOption
      .get

  private def undirectedMulti(
      key: String,
      labels: Map[Int, String],
      edges: Vector[(Int, Int, String)]
  ) =
    NeighbourhoodSample
      .undirected(SampleKey(key), labels.keys.toVector.sorted, labels, edges)
      .toOption
      .get

  private def kernelValue(
      left: LabelledNeighbourhood[Int, String, String],
      right: LabelledNeighbourhood[Int, String, String]
  ): Double =
    val prepared =
      WLCompiler
        .plan(Vector(left, right), refinement().subtree.normalized)
        .run()
        .toOption
        .get
    prepared.kernel(0, 1).get

  /** Colour index multisets per level for two samples traced together (indices are trace-local). */
  private def colourPair(
      left: LabelledNeighbourhood[Int, String, String],
      right: LabelledNeighbourhood[Int, String, String],
      rounds: Int = 2
  ): (Vector[Vector[Int]], Vector[Vector[Int]]) =
    val trace = WLTrace.prepare(Vector(left, right), refinement(rounds)).toOption.get
    val levels = (0 to rounds).toVector
    (
      levels.map(level => trace.graphColours(0, level).get.map(_.index).sorted),
      levels.map(level => trace.graphColours(1, level).get.map(_.index).sorted)
    )

  // Predicate-argument oracle: pred -ARG0-> a, pred -ARG1-> b, with a and b distinguishable.
  private val predicateLabels = Map(0 -> "strike", 1 -> "warriors", 2 -> "man")
  private val straight =
    directed("straight", predicateLabels, Vector((0, 1, "ARG0"), (0, 2, "ARG1")))
  private val swapped = directed("swapped", predicateLabels, Vector((0, 2, "ARG0"), (0, 1, "ARG1")))

  test("G1: swapping fillers across differently keyed arcs changes the kernel"):
    assert(kernelValue(straight, swapped) < 1.0 - 1e-9)
    val copy = directed("copy", predicateLabels, Vector((0, 1, "ARG0"), (0, 2, "ARG1")))
    assertEqualsDouble(kernelValue(straight, copy), 1.0, 1e-12)

  test("G1: reversing a directed edge changes the WL colours from round 1"):
    val labels = Map(0 -> "a", 1 -> "b", 2 -> "c")
    val path = directed("path", labels, Vector((0, 1, "e"), (1, 2, "e")))
    val reversedMiddle = directed("reversed", labels, Vector((1, 0, "e"), (1, 2, "e")))
    assert(kernelValue(path, reversedMiddle) < 1.0 - 1e-9)

  test("parallel keyed incidences refine to different colours than a single one"):
    val labels = Map(0 -> "x", 1 -> "y")
    val single = undirectedMulti("single", labels, Vector((0, 1, "e")))
    val double = undirectedMulti("double", labels, Vector((0, 1, "e"), (0, 1, "e")))
    assert(kernelValue(single, double) < 1.0 - 1e-9)
    // At round 0 the colours coincide (same labels); at round 1 they differ.
    val (singleColours, doubleColours) = colourPair(single, double)
    assertEquals(singleColours(0), doubleColours(0))
    assertNotEquals(singleColours(1), doubleColours(1))

  test("a self-loop counts once and distinguishes the vertex"):
    val labels = Map(0 -> "x", 1 -> "x")
    val plain = undirectedMulti("plain", labels, Vector((0, 1, "e")))
    val looped = undirectedMulti("looped", labels, Vector((0, 1, "e"), (0, 0, "e")))
    assert(kernelValue(plain, looped) < 1.0 - 1e-9)
    val packed = PackedGraphCorpus.pack(Vector(looped)).toOption.get
    assertEquals(packed.edgeCount, 2)
    assertEquals(packed.neighbors(0).toVector, Vector(1, 0))

  test("fingerprint: the WL codec schema is v2, so undirected-era artifacts cannot be reused"):
    val codecs =
      WLCodecs[String, String](
        Some(
          StableCodec
            .named(StableId.from("n").toOption.get, StableVersion.from(1).toOption.get)(identity)
        ),
        Some(
          StableCodec.named(StableId.from("e").toOption.get, StableVersion.from(1).toOption.get)(
            identity
          )
        )
      )
    val fingerprint = codecs.fingerprintability(PlanNodeId.fresh())
    fingerprint match
      case Fingerprintability.Durable(_) =>
        ()
      case other =>
        fail(s"expected a durable fingerprint, observed $other")

  private val genDigraph: Gen[(Map[Int, String], Vector[(Int, Int, String)])] =
    for
      n <- Gen.choose(2, 6)
      labels <- Gen.listOfN(n, Gen.oneOf("a", "b", "c"))
      m <- Gen.choose(1, 8)
      arcs <- Gen.listOfN(
        m,
        for
          s <- Gen.choose(0, n - 1)
          t <- Gen.choose(0, n - 1)
          k <- Gen.oneOf("e", "f")
        yield (s, t, k)
      )
    yield ((0 until n).zip(labels).toMap, arcs.toVector)

  private given Arbitrary[(Map[Int, String], Vector[(Int, Int, String)])] = Arbitrary(genDigraph)

  property("G2: alpha-renaming vertices and shuffling arc order leaves features bit-identical"):
    forAll { (input: (Map[Int, String], Vector[(Int, Int, String)]), seed: Long) =>
      val (labels, arcs) = input
      val n = labels.size
      val rng = new scala.util.Random(seed)
      val permutation = rng.shuffle((0 until n).toVector)
      val renamedLabels = labels.map { case (v, l) => permutation(v) -> l }
      val renamedArcs = rng.shuffle(arcs.map { case (s, t, k) =>
        (permutation(s), permutation(t), k)
      })
      val original = directed("g", labels, arcs)
      val renamed = directed("g", renamedLabels, renamedArcs)
      val left = WLCompiler.plan(Vector(original), refinement().subtree).run().toOption.get
      val right = WLCompiler.plan(Vector(renamed), refinement().subtree).run().toOption.get
      left.squaredNorm(0) == right.squaredNorm(0) && kernelValue(original, renamed) > 1.0 - 1e-12
    }
