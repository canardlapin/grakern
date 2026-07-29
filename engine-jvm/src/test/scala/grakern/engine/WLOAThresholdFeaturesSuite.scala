package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLOAThresholdFeaturesSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def fixture: (
      WLTrace[AtomId, String, Int],
      WLObservedHierarchy[String, Int]
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
          3,
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
    trace -> WLObservedHierarchy.fromTrace(trace).toOption.get

  test("threshold feature dots reproduce hierarchy intersection"):
    val (_, observed) = fixture
    val features = WLOAThresholdFeatures.fromObserved(observed)
    val intersection = WLOAIntersection(observed)

    observed.graphBags.indices.foreach { left =>
      observed.graphBags.indices.foreach { right =>
        assertEqualsDouble(
          features.kernel(left, right).get,
          intersection.evaluate(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }

  test("active threshold coordinates grow at most vertices times depth"):
    val (trace, observed) = fixture
    val features = WLOAThresholdFeatures.fromObserved(observed)
    val depth = trace.rounds + 1

    observed.graphBags.indices.foreach { graphIndex =>
      val bound = observed.graphBags(graphIndex).size * depth
      assert(
        features.activeCoordinates(graphIndex).get <= bound,
        clues(graphIndex, bound)
      )
    }

  test("threshold rows share the original refinement node"):
    val (trace, observed) = fixture
    val features = WLOAThresholdFeatures.fromObserved(observed)

    assertEquals(
      features.refinementNodeId.number,
      trace.refinementNodeId.number
    )
