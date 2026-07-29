package grakern.graph4s

import cats.Hash
import grakern.core.SampleKey
import graph4s.{Graph, Link}
import graph4s.data.{EdgeField, VertexField}
import munit.FunSuite

final class GraphSampleSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode

  test("equal atom values remain distinct graph vertices"):
    val left = AtomId(1)
    val right = AtomId(2)
    val topology = Graph.fromEdges(Link(left, right)).toOption.get
    val carbon = Atom("C")
    val sample =
      GraphSample.build(SampleKey("ethane"), topology)(
        nodeValue = _ => carbon,
        edgeValue = _ => Bond(1)
      )

    assertEquals(sample.topology.vertexCount, 2)
    assertEquals(sample.nodeValue(left), Some(carbon))
    assertEquals(sample.nodeValue(right), Some(carbon))
    assertEquals(sample.nodes.size, 2)

  test("different samples may reuse graph-local vertex identifiers"):
    val topology = Graph.singleton(AtomId(1))
    val first =
      GraphSample.build(SampleKey("first"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val second =
      GraphSample.build(SampleKey("second"), topology)(
        nodeValue = _ => Atom("N"),
        edgeValue = _ => Bond(1)
      )

    assertEquals(first.nodeValue(AtomId(1)), Some(Atom("C")))
    assertEquals(second.nodeValue(AtomId(1)), Some(Atom("N")))

  test("field-based construction rejects a node field from another topology"):
    val expected = Graph.singleton(AtomId(1))
    val other = Graph.singleton(AtomId(2))
    val nodes = VertexField.total(other)(_ => Atom("C"))
    val edges = EdgeField.total(expected)(_ => Bond(1))

    val result =
      GraphSample.fromFields(SampleKey("mismatch"), expected, nodes, edges)

    assert(
      result.exists(_ => false) ||
        result.left.exists(
          _.isInstanceOf[GraphSampleError.NodeFieldTopologyMismatch[AtomId]]
        )
    )

  test("field-based construction accepts fields scoped to the topology"):
    val topology = Graph.singleton(AtomId(1))
    val nodes = VertexField.total(topology)(_ => Atom("O"))
    val edges = EdgeField.total(topology)(_ => Bond(2))

    val result =
      GraphSample.fromFields(SampleKey("oxygen"), topology, nodes, edges)

    assertEquals(result.map(_.nodeValue(AtomId(1))), Right(Some(Atom("O"))))
