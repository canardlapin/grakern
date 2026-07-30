package grakern.engine

import cats.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import graph4s.{Graph, Link}
import munit.FunSuite

final class PackedGraphCorpusSuite extends FunSuite:
  final case class Atom(element: String, charge: Double)
  final case class Bond(order: Int)

  given Hash[Int] = Hash.fromUniversalHashCode

  test("packing scopes reused vertex ids by sample"):
    val firstTopology = Graph.fromEdges(Link(1, 2)).toOption.get
    val secondTopology = Graph.singleton(1)
    val first =
      GraphSample.build(SampleKey("first"), firstTopology)(
        nodeValue = id => Atom(s"A$id", id.toDouble),
        edgeValue = _ => Bond(1)
      )
    val second =
      GraphSample.build(SampleKey("second"), secondTopology)(
        nodeValue = _ => Atom("B1", 10.0),
        edgeValue = _ => Bond(1)
      )

    val packed = PackedGraphCorpus.pack(Vector(first, second)).toOption.get

    assertEquals(packed.graphCount, 2)
    assertEquals(packed.vertexCount, 3)
    assertEquals(packed.edgeCount, 1)
    assertEquals(packed.graphVertexRange(0).size, 2)
    assertEquals(packed.graphVertexRange(1).size, 1)
    assertEquals(
      packed.graphVertexRange(0).map(packed.ownerOfVertex).distinct,
      Seq(SampleKey("first"))
    )
    assertEquals(
      packed.graphVertexRange(1).map(packed.ownerOfVertex).distinct,
      Seq(SampleKey("second"))
    )

  test("CSR adjacency retains both directions of an undirected edge"):
    val topology = Graph.fromEdges(Link(1, 2)).toOption.get
    val sample =
      GraphSample.build(SampleKey("edge"), topology)(
        nodeValue = id => Atom(s"A$id", 0.0),
        edgeValue = _ => Bond(2)
      )
    val packed = PackedGraphCorpus.pack(Vector(sample)).toOption.get
    val degrees =
      packed.graphVertexRange(0).map(index => packed.neighbors(index).size)

    assertEquals(degrees.sorted, Seq(1, 1))
    assertEquals(packed.edgeCount, 1)

  test("categorical projections are accumulated once in packed order"):
    val topology = Graph.fromEdges(Link(1, 2)).toOption.get
    val sample =
      GraphSample.build(SampleKey("labels"), topology)(
        nodeValue = id => Atom(s"E$id", 0.0),
        edgeValue = _ => Bond(1)
      )
    val packed = PackedGraphCorpus.pack(Vector(sample)).toOption.get

    assertEquals(
      packed.categoricalNodeColumn(_.element).sorted,
      Vector("E1", "E2")
    )

  test("numeric projection errors retain sample, vertex, and descriptor"):
    val topology = Graph.singleton(7)
    val sample =
      GraphSample.build(SampleKey("bad-numeric"), topology)(
        nodeValue = _ => Atom("C", Double.NaN),
        edgeValue = _ => Bond(1)
      )
    val packed = PackedGraphCorpus.pack(Vector(sample)).toOption.get
    val result =
      packed.numericNodeColumn(ProjectionDescriptor("atom.charge"))(_.charge)

    result match
      case Left(
            PrepareError.InvalidNumeric(
              SourceRef.Node(sample, vertex),
              projection,
              value,
              NumericError.NonFinite(_)
            )
          ) =>
        assertEquals(sample, SampleKey("bad-numeric"))
        assertEquals(vertex, 7)
        assertEquals(projection, ProjectionDescriptor("atom.charge"))
        assert(value.isNaN)
      case other =>
        fail(s"expected non-finite node diagnostic, observed $other")

  test("finite numeric projections use a primitive column"):
    val topology = Graph.singleton(3)
    val sample =
      GraphSample.build(SampleKey("finite"), topology)(
        nodeValue = _ => Atom("N", -0.25),
        edgeValue = _ => Bond(1)
      )
    val packed = PackedGraphCorpus.pack(Vector(sample)).toOption.get
    val result =
      packed.numericNodeColumn(ProjectionDescriptor("atom.charge"))(_.charge)

    assertEquals(result.map(_.size), Right(1))
    assertEquals(result.map(_(0)), Right(-0.25))

  test("edge numeric projections reject negative values with edge provenance"):
    final case class WeightedBond(length: Double)
    val topology = Graph.fromEdges(Link(1, 2)).toOption.get
    val sample =
      GraphSample.build(SampleKey("negative-edge"), topology)(
        nodeValue = id => Atom(s"A$id", 0.0),
        edgeValue = _ => WeightedBond(-0.25)
      )
    val packed = PackedGraphCorpus.pack(Vector(sample)).toOption.get
    val result =
      packed.nonNegativeEdgeColumn(
        ProjectionDescriptor("bond.length")
      )(_.length)

    result match
      case Left(
            PrepareError.InvalidNumeric(
              SourceRef.Edge(sample, edge),
              projection,
              value,
              NumericError.Negative(_)
            )
          ) =>
        assertEquals(sample, SampleKey("negative-edge"))
        assertEquals(edge, topology.edges.head)
        assertEquals(
          projection,
          ProjectionDescriptor("bond.length")
        )
        assertEquals(value, -0.25)
      case other =>
        fail(s"expected negative edge diagnostic, observed $other")

  test("finite nonnegative edge projections use a primitive column"):
    final case class WeightedBond(length: Double)
    val topology =
      Graph.fromEdges(Link(1, 2), Link(2, 3)).toOption.get
    val sample =
      GraphSample.build(SampleKey("weighted"), topology)(
        nodeValue = id => Atom(s"A$id", 0.0),
        edgeValue = edge =>
          if edge.contains(1) then WeightedBond(0.0)
          else WeightedBond(1.5)
      )
    val packed = PackedGraphCorpus.pack(Vector(sample)).toOption.get
    val result =
      packed.nonNegativeEdgeColumn(
        ProjectionDescriptor("bond.length")
      )(_.length)

    assertEquals(result.map(_.size), Right(2))
    assertEquals(
      result.map(column => Vector.tabulate(column.size)(column.apply).sorted),
      Right(Vector(0.0, 1.5))
    )
