package grakern.engine

import grakern.core.*
import grakern.laws.{GraphGens, LabelledGraph, WLLaws, WLReference}
import grakern.standard.wl.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** Ties the portable law oracle to the JVM engine: every WL program the laws evaluate through
  * [[WLReference]] must agree with `WLCompiledKernel.kernel` on generated undirected and directed
  * graph sets. This is what lets the laws module make numerical claims on JS and Native.
  */
final class ReferenceParitySuite extends ScalaCheckSuite:
  import GraphGens.given

  private val rounds: Gen[Int] = Gen.choose(0, 3)

  private def programs(r: WLLaws.Ref): Vector[(String, WLKernel[String, String, String, String])] =
    Vector(
      "subtree" -> r.subtree,
      "oa" -> r.optimalAssignment,
      "subtree+oa" -> (r.subtree + r.optimalAssignment),
      "normalized subtree" -> r.subtree.normalized,
      "normalized oa" -> r.optimalAssignment.normalized,
      "weighted sum normalized" ->
        (r.subtree.weighted(
          NonNegativeFinite.from(0.5).toOption.get
        ) + r.optimalAssignment).normalized
    )

  private def parity(graphs: Vector[LabelledGraph], n: Int): Boolean =
    val r = WLLaws.refinement(n)
    val samples: Vector[LabelledNeighbourhood[Int, String, String]] =
      graphs.zipWithIndex.map { case (g, i) => g.sample(s"g$i") }
    val reference = WLReference.trace(samples, r)
    programs(r).forall { case (name, program) =>
      val compiled = WLCompiler.plan(samples, program).run().toOption.get
      graphs.indices.forall(i =>
        graphs.indices.forall { j =>
          val engine = compiled.kernel(i, j).get
          val oracle = reference.evaluate(program)(i, j)
          val ok = math.abs(engine - oracle) <= 1e-9 * math.max(1.0, math.abs(engine))
          if !ok then println(s"parity failure [$name] ($i,$j): engine=$engine oracle=$oracle")
          ok
        }
      )
    }

  property("undirected: reference evaluator equals the compiled kernel"):
    forAll(GraphGens.graphSet(directed = false), rounds)(parity)

  property("directed: reference evaluator equals the compiled kernel"):
    forAll(GraphGens.graphSet(directed = true), rounds)(parity)

  test("engine gram is positive semidefinite on a directed set (JVM eigen check)"):
    val graphs =
      Vector(
        LabelledGraph(Vector("a", "b", "c"), Vector((0, 1, "e"), (1, 2, "e")), directed = true),
        LabelledGraph(Vector("a", "b", "c"), Vector((1, 0, "e"), (1, 2, "e")), directed = true),
        LabelledGraph(Vector("a", "a"), Vector((0, 1, "f"), (1, 0, "f")), directed = true)
      )
    val r = WLLaws.refinement(2)
    val samples: Vector[LabelledNeighbourhood[Int, String, String]] =
      graphs.zipWithIndex.map { case (g, i) => g.sample(s"g$i") }
    val compiled = WLCompiler.plan(samples, r.subtree + r.optimalAssignment).run().toOption.get
    val gram = Array.tabulate(3, 3)((i, j) => compiled.kernel(i, j).get)
    assert(grakern.laws.SymmetricEigen.minEigenvalue(gram) >= -1e-9)
