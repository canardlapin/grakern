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
    assertEquals(sample.nodeValueOption(left), Some(carbon))
    assertEquals(sample.nodeValueOption(right), Some(carbon))
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

    assertEquals(first.nodeValueOption(AtomId(1)), Some(Atom("C")))
    assertEquals(second.nodeValueOption(AtomId(1)), Some(Atom("N")))

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

    assertEquals(result.map(_.nodeValueOption(AtomId(1))), Right(Some(Atom("O"))))

  test("edge-list construction rejects a self-loop with a typed error"):
    val atom = AtomId(1)
    val result =
      GraphSample.fromEdgeList(
        SampleKey("loop"),
        Vector(atom),
        Vector(GraphSampleEdge(atom, atom, Bond(1)))
      )(_ => Atom("C"))

    assertEquals(
      result,
      Left(GraphSampleEdgeListError.SelfLoopUnsupported(0, atom))
    )

  test("edge-list construction rejects parallel and reversed duplicate edges"):
    val left = AtomId(1)
    val right = AtomId(2)
    val result =
      GraphSample.fromEdgeList(
        SampleKey("parallel"),
        Vector(left, right),
        Vector(
          GraphSampleEdge(left, right, Bond(1)),
          GraphSampleEdge(right, left, Bond(2))
        )
      )(_ => Atom("C"))

    assertEquals(
      result,
      Left(
        GraphSampleEdgeListError.ParallelEdgeUnsupported(
          edgeIndex = 1,
          previousEdgeIndex = 0,
          left = right,
          right = left
        )
      )
    )

  test("edge-list construction preserves every accepted edge value"):
    val first = AtomId(1)
    val second = AtomId(2)
    val third = AtomId(3)
    val result =
      GraphSample
        .fromEdgeList(
          SampleKey("path"),
          Vector(first, second, third),
          Vector(
            GraphSampleEdge(first, second, Bond(1)),
            GraphSampleEdge(second, third, Bond(2))
          )
        )(_ => Atom("C"))
        .toOption
        .get

    assertEquals(result.edgeValue(first, second), Some(Bond(1)))
    assertEquals(result.edgeValue(second, third), Some(Bond(2)))
    assertEquals(result.topology.edgeCount, 2L)
