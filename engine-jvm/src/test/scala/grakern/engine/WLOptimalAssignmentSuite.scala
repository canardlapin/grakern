package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLOptimalAssignmentSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def observed: WLObservedHierarchy[String, Int] =
    val singleton =
      GraphSample.build(
        SampleKey("singleton"),
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
    val pathTopology =
      Graph
        .fromEdges(
          Link(AtomId(100), AtomId(200)),
          Link(AtomId(200), AtomId(300))
        )
        .toOption
        .get
    val path =
      GraphSample.build(SampleKey("path"), pathTopology)(
        nodeValue = id => Atom(if id.value == 200 then "O" else "C"),
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
        .prepare(Vector(singleton, pair, path), refinement)
        .toOption
        .get
    WLObservedHierarchy.fromTrace(trace).toOption.get

  test("the padded oracle handles unequal graph bags"):
    val value = observed
    val oracle = WLOptimalAssignmentOracle(value)

    assertEquals(value.bag(0).map(_.size), Some(1))
    assertEquals(value.bag(1).map(_.size), Some(2))
    assertEquals(oracle.evaluate(0, 1), Some(1.0))
    assertEquals(oracle.evaluate(1, 0), Some(1.0))

  test("repeated final colours retain their assignment multiplicity"):
    val oracle = WLOptimalAssignmentOracle(observed)

    assertEquals(oracle.evaluate(1, 1), Some(6.0))

  test("the lawful OA kernel expression agrees with the direct oracle"):
    val value = observed
    val expression = OptimalAssignment(value.kernel)
    val oracle = WLOptimalAssignmentOracle(value)

    value.graphBags.indices.foreach { left =>
      value.graphBags.indices.foreach { right =>
        assertEqualsDouble(
          expression.evaluate(
            value.graphBags(left),
            value.graphBags(right)
          ),
          oracle.evaluate(left, right).get,
          1e-12
        )
      }
    }
