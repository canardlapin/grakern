package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLCompilerSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private val subtreeWeight =
    NonNegativeFinite.from(0.7).toOption.get
  private val assignmentWeight =
    NonNegativeFinite.from(0.3).toOption.get

  private def samples: Vector[GraphSample[AtomId, Atom, Bond]] =
    val pairTopology =
      Graph
        .fromEdges(Link(AtomId(1), AtomId(2)))
        .toOption
        .get
    val pathTopology =
      Graph
        .fromEdges(
          Link(AtomId(10), AtomId(20)),
          Link(AtomId(20), AtomId(30))
        )
        .toOption
        .get
    Vector(
      GraphSample.build(SampleKey("pair"), pairTopology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      ),
      GraphSample.build(SampleKey("path"), pathTopology)(
        nodeValue = id => Atom(if id.value == 20 then "O" else "C"),
        edgeValue = _ => Bond(1)
      )
    )

  private def refinement =
    WLRefinement
      .create(
        2,
        Projection.of[Atom, String](_.element),
        Projection.of[Bond, Int](_.order),
        WLCodecs[String, Int](None, None)
      )
      .toOption
      .get

  test("paired WL consumers execute one shared refinement"):
    val shared = refinement
    val kernel =
      (
        shared.subtree.weighted(subtreeWeight) +
          shared.optimalAssignment.weighted(assignmentWeight)
      ).normalized
    val plan = WLCompiler.plan(samples, kernel)
    val compiled = plan.run().toOption.get

    assertEquals(
      plan.graph.count(WLPlanNodeKind.Refinement),
      1
    )
    assertEquals(
      plan.graph
        .consumers(shared.planNodeId)
        .map(_.kind)
        .toSet,
      Set(
        WLPlanNodeKind.SubtreeFeatures,
        WLPlanNodeKind.OptimalAssignmentThresholdFeatures
      )
    )
    assertEquals(
      compiled.executionStats,
      WLExecutionStats(1, 1, 1)
    )
    compiled.sampleKeys.indices.foreach { graphIndex =>
      assertEqualsDouble(
        compiled.squaredNorm(graphIndex).get,
        1.0,
        1e-12
      )
    }

  test("compiled paired features agree with the independent E3 realization"):
    val shared = refinement
    val source =
      WLTrace.prepare(samples, shared).toOption.get
    val expected =
      WLCombinedFeatures
        .fromTrace(
          source,
          Vector.fill(shared.rounds + 1)(
            NonNegativeFinite.one
          ),
          subtreeWeight,
          assignmentWeight,
          NonNegativeFinite.zero
        )
        .toOption
        .get
    val kernel =
      (
        shared.subtree.weighted(subtreeWeight) +
          shared.optimalAssignment.weighted(assignmentWeight)
      ).normalized
    val compiled =
      WLCompiler.plan(samples, kernel).run().toOption.get

    compiled.sampleKeys.indices.foreach { left =>
      compiled.sampleKeys.indices.foreach { right =>
        assertEqualsDouble(
          compiled.kernel(left, right).get,
          expected.kernel(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }

  test("a repeated child is represented as two direct-sum blocks"):
    val shared = refinement
    val subtree = shared.subtree
    val single =
      WLCompiler.plan(samples, subtree).run().toOption.get
    val doubled =
      WLCompiler.plan(samples, subtree + subtree).run().toOption.get

    doubled.sampleKeys.indices.foreach { left =>
      doubled.sampleKeys.indices.foreach { right =>
        assertEqualsDouble(
          doubled.kernel(left, right).get,
          2.0 * single.kernel(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }
    assertEquals(
      doubled.executionStats.refinementExecutions,
      1
    )

  test("zero weights preserve zero normalized diagonals"):
    val kernel =
      refinement.subtree
        .weighted(NonNegativeFinite.zero)
        .normalized
    val compiled =
      WLCompiler.plan(samples, kernel).run().toOption.get

    compiled.sampleKeys.indices.foreach { index =>
      assertEquals(compiled.kernel(index, index), Some(0.0))
    }
    assertEquals(
      compiled.auditEvents.size,
      compiled.sampleKeys.size
    )
