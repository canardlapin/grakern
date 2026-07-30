package grakern.engine

import cats.kernel.Hash
import gale.linalg.DVec
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.histogram.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class HistogramCompilerSuite extends FunSuite:
  final case class Atom(label: String)
  final case class Bond(order: Int)

  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode

  private def samples: Vector[GraphSample[Int, Atom, Bond]] =
    val pair =
      Graph
        .of(Vector(1, 2), Vector(Link(1, 2)))
        .toOption
        .get
    val path =
      Graph
        .of(
          Vector(10, 20, 30),
          Vector(Link(10, 20), Link(20, 30))
        )
        .toOption
        .get
    Vector(
      GraphSample.build(SampleKey("pair"), pair)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      ),
      GraphSample.build(SampleKey("path"), path)(
        nodeValue = vertex => Atom(if vertex == 20 then "O" else "C"),
        edgeValue = edge =>
          Bond(
            if edge.first == 10 || edge.second == 10 then 1
            else 2
          )
      )
    )

  test("vertex histogram uses decomposition-backed exact sparse features"):
    val recipe =
      Histograms.vertices[Atom, Bond, String](
        Projection.of(_.label)
      )
    val prepared =
      HistogramCompiler
        .prepare(samples, recipe)
        .toOption
        .get

    assertEqualsDouble(prepared.gram.values(0, 0), 4.0, 0.0)
    assertEqualsDouble(prepared.gram.values(0, 1), 4.0, 0.0)
    assertEqualsDouble(prepared.gram.values(1, 1), 5.0, 0.0)
    assertEquals(prepared.features.values.nnz, 3)
    assertEquals(prepared.planReport.emittedPatterns, 5)
    assert(
      prepared.planReport.explain.contains(
        "fused nonnegative pattern fold"
      )
    )

  test("edge histogram records complete graph-scoped provenance"):
    val recipe =
      Histograms.edges[Atom, Bond, Int](
        Projection.of(_.order)
      )
    val prepared =
      HistogramCompiler
        .prepare(samples, recipe)
        .toOption
        .get

    assertEqualsDouble(prepared.kernel(0, 0).get, 1.0, 0.0)
    assertEqualsDouble(prepared.kernel(0, 1).get, 1.0, 0.0)
    assertEqualsDouble(prepared.kernel(1, 1).get, 2.0, 0.0)
    assertEquals(
      prepared.provenance.map(_.witnesses.size).sum,
      3
    )
    assert(
      prepared.provenance
        .flatMap(_.witnesses)
        .forall {
          case HistogramWitness.Edge(_, _)   => true
          case HistogramWitness.Vertex(_, _) => false
        }
    )
    assertEqualsDouble(
      prepared
        .explain(0, 1)
        .get
        .map(_.value)
        .sum,
      prepared.kernel(0, 1).get,
      0.0
    )

  test("Gale operator agrees with histogram Gram multiplication"):
    val prepared =
      HistogramCompiler
        .prepare(
          samples,
          Histograms.vertices[Atom, Bond, String](
            Projection.of(_.label)
          )
        )
        .toOption
        .get
    val input = DVec.fromSeq(Vector(0.25, -0.5))
    val observed = prepared.operator.values(input)

    prepared.sampleKeys.indices.foreach { row =>
      val expected =
        prepared.sampleKeys.indices.map { column =>
          prepared.gram.values(row, column) *
            input(column)
        }.sum
      assertEqualsDouble(observed(row), expected, 1e-12)
    }

  test("vertex reindexing preserves histogram values"):
    val reindexed =
      val topology =
        Graph
          .of(
            Vector(100, 200),
            Vector(Link(100, 200))
          )
          .toOption
          .get
      GraphSample.build(SampleKey("reindexed"), topology)(
        nodeValue = _ => Atom("C"),
        edgeValue = _ => Bond(1)
      )
    val prepared =
      HistogramCompiler
        .prepare(
          samples.take(1) :+ reindexed,
          Histograms.vertices[Atom, Bond, String](
            Projection.of(_.label)
          )
        )
        .toOption
        .get

    assertEqualsDouble(prepared.kernel(0, 1).get, 4.0, 0.0)
