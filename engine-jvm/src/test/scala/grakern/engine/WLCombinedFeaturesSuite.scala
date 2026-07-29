package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLCombinedFeaturesSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def trace: WLTrace[AtomId, String, Int] =
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
    val pair =
      GraphSample.build(SampleKey("pair"), pairTopology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val path =
      GraphSample.build(SampleKey("path"), pathTopology)(
        nodeValue = id => Atom(if id.value == 20 then "O" else "C"),
        edgeValue = _ => Bond(1)
      )
    val refinement =
      WLRefinement
        .create(
          2,
          Projection.of[Atom, String](_.element),
          Projection.of[Bond, Int](_.order),
          WLCodecs[String, Int](None, None)
        )
        .toOption
        .get
    WLTrace
      .prepare(Vector(pair, path), refinement)
      .toOption
      .get

  private val subtreeWeight =
    NonNegativeFinite.from(0.7).toOption.get
  private val assignmentWeight =
    NonNegativeFinite.from(0.3).toOption.get

  test("direct sum dot products equal the weighted component sum"):
    val source = trace
    val combined =
      WLCombinedFeatures
        .fromTrace(
          source,
          Vector.fill(source.rounds + 1)(
            NonNegativeFinite.one
          ),
          subtreeWeight,
          assignmentWeight,
          NonNegativeFinite.zero
        )
        .toOption
        .get

    source.sampleKeys.indices.foreach { left =>
      source.sampleKeys.indices.foreach { right =>
        val expected =
          subtreeWeight.toDouble *
            combined.subtree.kernel(left, right).get +
            assignmentWeight.toDouble *
            combined.optimalAssignment.kernel(left, right).get
        assertEqualsDouble(
          combined.rawKernel(left, right).get,
          expected,
          1e-12,
          clues(left, right)
        )
      }
    }

  test("normalization produces unit nonzero diagonals"):
    val source = trace
    val combined =
      WLCombinedFeatures
        .fromTrace(
          source,
          Vector.fill(source.rounds + 1)(
            NonNegativeFinite.one
          ),
          subtreeWeight,
          assignmentWeight,
          NonNegativeFinite.zero
        )
        .toOption
        .get

    source.sampleKeys.indices.foreach { graphIndex =>
      assertEqualsDouble(
        combined.kernel(graphIndex, graphIndex).get,
        1.0,
        1e-12
      )
      assertEqualsDouble(
        combined.squaredNorm(graphIndex).get,
        1.0,
        1e-12
      )
    }

  test("zero combined weight keeps zero-vector diagonals at zero"):
    val source = trace
    val combined =
      WLCombinedFeatures
        .fromTrace(
          source,
          Vector.fill(source.rounds + 1)(
            NonNegativeFinite.one
          ),
          NonNegativeFinite.zero,
          NonNegativeFinite.zero,
          NonNegativeFinite.zero
        )
        .toOption
        .get

    source.sampleKeys.indices.foreach { graphIndex =>
      assertEquals(combined.kernel(graphIndex, graphIndex), Some(0.0))
      assertEquals(combined.squaredNorm(graphIndex), Some(0.0))
      assertEquals(combined.activeCoordinates(graphIndex), Some(0))
    }
    assertEquals(
      combined.auditEvents.size,
      source.sampleKeys.size
    )

  test("the combined plan retains one shared refinement node"):
    val source = trace
    val combined =
      WLCombinedFeatures
        .fromTrace(
          source,
          Vector.fill(source.rounds + 1)(
            NonNegativeFinite.one
          ),
          subtreeWeight,
          assignmentWeight,
          NonNegativeFinite.zero
        )
        .toOption
        .get

    assertEquals(
      combined.refinementNodeId.number,
      source.refinementNodeId.number
    )
    assertEquals(
      combined.subtree.refinementNodeId.number,
      source.refinementNodeId.number
    )
    assertEquals(
      combined.optimalAssignment.refinementNodeId.number,
      source.refinementNodeId.number
    )
