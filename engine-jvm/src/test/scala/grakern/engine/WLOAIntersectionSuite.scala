package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLOAIntersectionSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def observed: WLObservedHierarchy[String, Int] =
    val isolated =
      GraphSample.build(
        SampleKey("isolated"),
        Graph.singleton(AtomId(1))
      )(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val pairTopology =
      Graph
        .fromEdges(Link(AtomId(10), AtomId(20)))
        .toOption
        .get
    val pair =
      GraphSample.build(SampleKey("pair"), pairTopology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val mixedPair =
      GraphSample.build(SampleKey("mixed"), pairTopology)(
        nodeValue = id => Atom(if id.value == 10 then "C" else "O"),
        edgeValue = _ => Bond(2)
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
        .prepare(
          Vector(isolated, pair, mixedPair),
          refinement
        )
        .toOption
        .get
    WLObservedHierarchy.fromTrace(trace).toOption.get

  test("hierarchy intersection agrees with Hungarian on every pair"):
    val value = observed
    val intersection = WLOAIntersection(value)
    val oracle = WLOptimalAssignmentOracle(value)

    value.graphBags.indices.foreach { left =>
      value.graphBags.indices.foreach { right =>
        assertEqualsDouble(
          intersection.evaluate(left, right).get,
          oracle.evaluate(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }

  test("intersection and oracle retain the same refinement identity"):
    val value = observed
    val intersection = WLOAIntersection(value)
    val oracle = WLOptimalAssignmentOracle(value)

    assertEquals(
      intersection.refinementNodeId.number,
      oracle.refinementNodeId.number
    )
