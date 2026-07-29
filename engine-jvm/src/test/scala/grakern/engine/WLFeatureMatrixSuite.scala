package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import munit.FunSuite

final class WLFeatureMatrixSuite extends FunSuite:
  final case class AtomId(value: Int)
  final case class Atom(element: String)
  final case class Bond(order: Int)

  given Hash[AtomId] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Hash[Int] = Hash.fromUniversalHashCode

  private def compiled =
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
    val samples =
      Vector(
        GraphSample.build(SampleKey("pair"), pairTopology)(
          nodeValue = _ => Atom("C"),
          edgeValue = _ => Bond(1)
        ),
        GraphSample.build(SampleKey("path"), pathTopology)(
          nodeValue = id => Atom(if id.value == 20 then "O" else "C"),
          edgeValue = _ => Bond(1)
        )
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
    val kernel =
      (
        refinement.subtree +
          refinement.optimalAssignment
      ).normalized
    WLCompiler.plan(samples, kernel).run().toOption.get

  test("the exact training realization is a Gale CSR with owned identity"):
    val prepared = compiled
    val features = prepared.features

    assertEquals(features.values.rows, prepared.graphCount)
    assertEquals(
      features.values.cols,
      features.columns.size
    )
    assertEquals(
      features.rows.keys,
      prepared.sampleKeys
    )
    assertEquals(
      features.metadata.columns.size,
      features.columns.size
    )
    assert(features.values.nnz > 0)

  test("CSR row dot products reproduce the compiled kernel"):
    val prepared = compiled
    val dense = prepared.features.values.toDense()

    prepared.sampleKeys.indices.foreach { left =>
      prepared.sampleKeys.indices.foreach { right =>
        var expected = 0.0
        var column = 0
        while column < dense.cols do
          expected += dense(left, column) * dense(right, column)
          column += 1
        assertEqualsDouble(
          expected,
          prepared.kernel(left, right).get,
          1e-12,
          clues(left, right)
        )
      }
    }

  test("every dictionary column carries typed WL metadata"):
    val features = compiled.features

    features.columns.coordinates.indices.foreach { index =>
      val coordinate = features.columns.coordinate(index).get
      val metadata = features.metadata.columns(index)
      (coordinate.feature, metadata) match
        case (
              WLCompiledFeature.Subtree(feature),
              WLFeatureMetadata.Subtree(path, described)
            ) =>
          assertEquals(path, coordinate.blockPath)
          assert(
            WLSubtreeFeatures.sameFeature(feature, described)
          )
        case (
              WLCompiledFeature.OptimalAssignment(feature),
              WLFeatureMetadata.OptimalAssignment(
                path,
                described
              )
            ) =>
          assertEquals(path, coordinate.blockPath)
          assert(
            WLOAThresholdFeatures.sameFeature(
              feature,
              described
            )
          )
        case mismatch =>
          fail(s"metadata mismatch: $mismatch")
    }
