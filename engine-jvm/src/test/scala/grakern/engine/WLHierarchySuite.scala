package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLHierarchySuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def trace: WLTrace[AtomId, String, Int] =
    val topology =
      Graph
        .fromEdges(
          Link(AtomId(1), AtomId(2)),
          Link(AtomId(2), AtomId(3))
        )
        .toOption
        .get
    val sample =
      GraphSample.build(SampleKey("chain"), topology)(
        nodeValue = id => Atom(if id.value == 2 then "O" else "C"),
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
    WLTrace.prepare(Vector(sample), refinement).toOption.get

  test("the observed hierarchy has a zero root and adjacent parents"):
    val source = trace
    val observed =
      WLObservedHierarchy.fromTrace(source).toOption.get

    assertEquals(
      observed.hierarchy
        .heightOf(WLHierarchyNode.Root)
        .map(_.toDouble),
      Some(0.0)
    )
    source.interner.colours.foreach { colour =>
      val node = WLHierarchyNode.Signature(colour)
      val expected =
        source.signature(colour).flatMap(_.parent) match
          case Some(parent) =>
            Some(WLHierarchyNode.Signature(parent))
          case None =>
            Some(WLHierarchyNode.Root)
      assertEquals(observed.hierarchy.parentOf(node), expected)
    }

  test("final colours are leaves and repeated vertices remain bag counts"):
    val source = trace
    val observed =
      WLObservedHierarchy.fromTrace(source).toOption.get
    val bag = observed.bag(0).get
    val endpointColours =
      (0 until source.vertexCount)
        .filter(index => source.source(index).exists(_.vertex.value != 2))
        .map(index => source.colour(source.rounds, index).get)

    assertEquals(bag.size, 3)
    assertEquals(endpointColours.size, 2)
    assertEquals(endpointColours.head, endpointColours.last)
    assertEquals(
      bag.items.count(_.equals(endpointColours.head)),
      2
    )
    assertEquals(
      observed.hierarchy.leafOf(endpointColours.head),
      Some(WLHierarchyNode.Signature(endpointColours.head))
    )

  test("WL hierarchy kernels expose histogram-intersection capability"):
    val source = trace
    val observed =
      WLObservedHierarchy.fromTrace(source).toOption.get
    val endpoint =
      (0 until source.vertexCount)
        .find(index => source.source(index).exists(_.vertex.value == 1))
        .flatMap(source.colour(source.rounds, _))
        .get

    assertEquals(
      observed.kernel.optimalAssignmentRealization,
      Some(StrongKernelRealization.HierarchyIntersection)
    )
    assertEquals(
      observed.kernel.evaluate(endpoint, endpoint),
      3.0
    )
