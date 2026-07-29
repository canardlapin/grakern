package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLFeatureProvenanceSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def fixture: (
      WLTrace[AtomId, String, Int],
      WLCombinedFeatures[AtomId, String, Int]
  ) =
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
    val trace =
      WLTrace
        .prepare(Vector(pair, path), refinement)
        .toOption
        .get
    val combined =
      WLCombinedFeatures
        .fromTrace(
          trace,
          Vector.fill(trace.rounds + 1)(
            NonNegativeFinite.one
          ),
          NonNegativeFinite.one,
          NonNegativeFinite.one,
          NonNegativeFinite.zero
        )
        .toOption
        .get
    trace -> combined

  test("every active combined coordinate has complete provenance"):
    val (_, combined) = fixture
    val active =
      combined.rawRows.iterator
        .flatMap(_.entries.iterator.map(_._1))
        .toVector

    active.foreach { feature =>
      val found = combined.provenance.find(feature).get
      assert(found.exactSignature.nonEmpty)
      assert(found.round.nonEmpty)
      assert(found.representatives.nonEmpty)
      feature match
        case WLCombinedFeature.Subtree(_) =>
          assertEquals(
            found.component,
            WLFeatureComponent.Subtree
          )
          assertEquals(found.hierarchyNode, None)
          assertEquals(found.multiplicityThreshold, None)
        case WLCombinedFeature.OptimalAssignment(value) =>
          assertEquals(
            found.component,
            WLFeatureComponent.OptimalAssignment
          )
          assert(
            found.hierarchyNode.exists(
              WLOAThresholdFeatures.sameNode(_, value.node)
            )
          )
          assertEquals(
            found.multiplicityThreshold,
            Some(value.multiplicityThreshold)
          )
    }

  test("feature contributions exactly reconstruct similarity"):
    val (_, combined) = fixture

    combined.sampleKeys.indices.foreach { left =>
      combined.sampleKeys.indices.foreach { right =>
        val contributions =
          combined.explain(left, right).get
        assertEqualsDouble(
          contributions.map(_.value).sum,
          combined.kernel(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }

  test("OA threshold provenance names supporting final vertices"):
    val (_, combined) = fixture
    val assignment =
      combined.provenance.entries.filter(
        _.component == WLFeatureComponent.OptimalAssignment
      )

    assert(assignment.nonEmpty)
    assert(
      assignment.forall(_.hierarchyNode.nonEmpty)
    )
    assert(
      assignment.forall(
        _.multiplicityThreshold.exists(_ >= 1)
      )
    )
    assert(
      assignment.forall(_.representatives.nonEmpty)
    )
