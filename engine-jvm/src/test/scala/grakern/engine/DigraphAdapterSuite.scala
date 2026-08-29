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

  private def bits(value: Double): Long =
    java.lang.Double.doubleToLongBits(value)

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
    val (straightColours, swappedColours) = colourPair(straight, swapped)
    assertNotEquals(straightColours(1), swappedColours(1))
    assert(kernelValue(straight, swapped) < 1.0 - 1e-9)
    val copy = directed("copy", predicateLabels, Vector((0, 1, "ARG0"), (0, 2, "ARG1")))
    assertEqualsDouble(kernelValue(straight, copy), 1.0, 1e-12)

  test("G1: reversing a directed edge changes the WL colours from round 1"):
    val labels = Map(0 -> "a", 1 -> "b", 2 -> "c")
    val path = directed("path", labels, Vector((0, 1, "e"), (1, 2, "e")))
    val reversedMiddle = directed("reversed", labels, Vector((1, 0, "e"), (1, 2, "e")))
    val (pathColours, reversedColours) = colourPair(path, reversedMiddle)
    assertNotEquals(pathColours(1), reversedColours(1))
    assert(kernelValue(path, reversedMiddle) < 1.0 - 1e-9)

  test("G1: two outgoing star edges differ from one incoming and one outgoing edge"):
    val labels = Map(0 -> "hub", 1 -> "left", 2 -> "right")
    val outgoing = directed("outgoing", labels, Vector((0, 1, "e"), (0, 2, "e")))
    val mixed = directed("mixed", labels, Vector((0, 1, "e"), (2, 0, "e")))
    val (outgoingColours, mixedColours) = colourPair(outgoing, mixed)

    assertNotEquals(outgoingColours(1), mixedColours(1))
    assert(kernelValue(outgoing, mixed) < 1.0 - 1e-9)

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
      val lawRefinement = refinement()
      val kernel = lawRefinement.subtree.normalized
      val paired = WLCompiler.plan(Vector(original, renamed), kernel).run().toOption.get
      val training = WLCompiler.plan(Vector(original), kernel).run().toOption.get
      val query = training.cross(Vector(renamed)).toOption.get
      val trainingFeatures = training.features.values.toDense()
      val queryFeatures = query.features.shared.toDense()
      val (originalColours, renamedColours) = colourPair(original, renamed)

      val checks =
        Vector(
          "colour multisets" -> (originalColours == renamedColours),
          "Gram diagonals" ->
            (bits(paired.gram.values(0, 0)) == bits(paired.gram.values(1, 1))),
          "Gram cross" ->
            (bits(paired.gram.values(0, 0)) == bits(paired.gram.values(0, 1))),
          "query cross" ->
            (bits(query.cross.values(0, 0)) == bits(training.gram.values(0, 0))),
          "query novel norm" ->
            (bits(query.features.novelSquaredNorm(0)) == bits(0.0)),
          "query sparse features" ->
            (0 until trainingFeatures.cols).forall { column =>
              bits(queryFeatures(0, column)) == bits(trainingFeatures(0, column))
            }
        )
      val failed = checks.collect { case (name, false) => name }
      assert(failed.isEmpty, clues(input, seed, failed))
      true
    }
