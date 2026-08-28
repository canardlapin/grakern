package grakern.laws

import cats.kernel.Eq
import grakern.core.*
import grakern.standard.wl.*
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}

/** Registers every rule set; runs on JVM, Scala.js and Scala Native. */
final class LawsSuite extends DisciplineSuite:
  given Eq[Int] = Eq.fromUniversalEquals
  given Eq[String] = Eq.fromUniversalEquals

  private given smallInts: Arbitrary[Int] = Arbitrary(Gen.choose(0, 4))
  private given smallVectors: Arbitrary[Vector[Double]] =
    Arbitrary(Gen.listOfN(3, Gen.choose(-2.0, 2.0)).map(_.toVector))
  private given smallDoubles: Arbitrary[Double] = Arbitrary(Gen.choose(-3.0, 3.0))

  private val gamma = PositiveFinite.from(0.7).toOption.get
  private val half = NonNegativeFinite.from(0.5).toOption.get

  // ---- core kernel families -------------------------------------------------------------
  checkAll("delta[Int]", KernelLaws.kernel("delta", Kernel.delta[Int]))
  checkAll("constant", KernelLaws.kernel("constant", Kernel.constant[Int](half)))
  checkAll("linear", KernelLaws.kernel("linear", Kernel.linear))
  checkAll("rbf", KernelLaws.kernel("rbf", Kernel.rbf(gamma)))
  checkAll(
    "delta + constant (sum closure)",
    KernelLaws.kernel("sum", Kernel.delta[Int] + Kernel.constant[Int](half))
  )
  checkAll(
    "linear * linear (product closure)",
    KernelLaws.kernel("product", Kernel.linear * Kernel.linear)
  )
  checkAll("rbf weighted", KernelLaws.kernel("weighted", Kernel.rbf(gamma).weighted(half)))
  checkAll("linear normalized", KernelLaws.kernel("normalized", Kernel.linear.normalized))
  checkAll(
    "core algebra",
    KernelLaws.algebra(
      "core",
      Kernel.delta[Int],
      Kernel.constant[Int](half),
      Kernel.delta[Int].weighted(half)
    )
  )
  checkAll("normalized linear", KernelLaws.normalized("linear", Kernel.linear, nonNegative = false))
  checkAll("normalized rbf", KernelLaws.normalized("rbf", Kernel.rbf(gamma), nonNegative = true))
  checkAll("weights", KernelLaws.weights)

  // ---- WL kernel programs (portable reference evaluator) --------------------------------
  private val subtree: WLLaws.Program = _.subtree
  private val optimalAssignment: WLLaws.Program = _.optimalAssignment
  private val both: WLLaws.Program = r => r.subtree + r.optimalAssignment
  private val weightedSubtree: WLLaws.Program = _.subtree.weighted(half)

  for directed <- List(false, true) do
    val tag = if directed then "directed" else "undirected"
    checkAll(s"wl subtree $tag", WLLaws.kernel("subtree", subtree, directed))
    checkAll(s"wl optimal-assignment $tag", WLLaws.kernel("oa", optimalAssignment, directed))
    checkAll(s"wl subtree+oa $tag", WLLaws.kernel("subtree+oa", both, directed))
    checkAll(s"wl weighted subtree $tag", WLLaws.kernel("weighted", weightedSubtree, directed))
    checkAll(s"wl normalized subtree $tag", WLLaws.normalized("subtree", subtree, directed))
    checkAll(s"wl normalized oa $tag", WLLaws.normalized("oa", optimalAssignment, directed))
    checkAll(s"wl normalized subtree+oa $tag", WLLaws.normalized("subtree+oa", both, directed))
    checkAll(s"wl algebra $tag", WLLaws.algebra(directed))
    checkAll(s"wl refinement $tag", WLLaws.refinementLaws(directed))
    checkAll(s"wl hierarchy $tag", WLLaws.hierarchyLaws(directed))

  checkAll("wl directed (G1, G2)", WLLaws.directedLaws)

  test("reference evaluator: hand oracle for the subtree kernel at rounds = 1"):
    // Two vertices a-b (edge e) versus a single a: round 0 shares one 'a'; round 1 differs.
    val pair = LabelledGraph(Vector("a", "b"), Vector((0, 1, "e")), directed = false)
    val single = LabelledGraph(Vector("a"), Vector.empty, directed = false)
    val r = WLLaws.refinement(1)
    val ref = WLReference.trace(Vector(pair.sample("p"), single.sample("s")), r)(using
      GraphGens.stringHash,
      GraphGens.stringHash
    )
    assertEqualsDouble(ref.subtree(Vector.fill(2)(NonNegativeFinite.one))(0, 1), 1.0, 1e-12)
    assertEqualsDouble(ref.subtree(Vector.fill(2)(NonNegativeFinite.one))(0, 0), 4.0, 1e-12)
    assertEqualsDouble(ref.optimalAssignment(0, 1), 1.0, 1e-12)
