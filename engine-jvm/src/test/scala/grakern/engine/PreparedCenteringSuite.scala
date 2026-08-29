package grakern.engine

import cats.kernel.{Eq, Hash}
import gale.linalg.DVec
import grakern.core.*
import grakern.standard.wl.*
import munit.FunSuite

final class PreparedCenteringSuite extends FunSuite:
  given Hash[Int] = Hash.fromUniversalHashCode
  given Hash[String] = Hash.fromUniversalHashCode
  given Eq[String] = Eq.fromUniversalEquals

  private def sample(
      key: String,
      labels: Map[Int, String],
      edges: Vector[(Int, Int, String)]
  ) =
    NeighbourhoodSample
      .undirected(
        SampleKey(key),
        labels.keys.toVector.sorted,
        labels,
        edges
      )
      .toOption
      .get

  private val training =
    Vector(
      sample(
        "cc",
        Map(0 -> "C", 1 -> "C"),
        Vector((0, 1, "single"))
      ),
      sample(
        "coc",
        Map(0 -> "C", 1 -> "O", 2 -> "C"),
        Vector((0, 1, "single"), (1, 2, "single"))
      ),
      sample(
        "oo",
        Map(0 -> "O", 1 -> "O"),
        Vector((0, 1, "double"))
      )
    )

  private val query =
    Vector(
      sample(
        "cnc",
        Map(0 -> "C", 1 -> "N", 2 -> "C"),
        Vector((0, 1, "single"), (1, 2, "single"))
      )
    )

  private def kernel =
    val refinement =
      WLRefinement
        .create(
          2,
          Projection.of[String, String](identity),
          Projection.of[String, String](identity),
          WLCodecs[String, String](None, None)
        )
        .toOption
        .get
    (refinement.subtree + refinement.optimalAssignment).normalized

  private def prepared =
    WLCompiler.plan(training, kernel).run().toOption.get

  test("training features and Gram are centered against the frozen reference"):
    val raw = prepared
    val centered = raw.centered.toOption.get
    val dense = centered.features.values.toDense()

    assertEquals(centered.statistics.reference.keys, raw.sampleKeys)
    assert(centered.planReport.explain.contains("reference-bound prepared centering"))
    (0 until dense.cols).foreach { column =>
      var total = 0.0
      var row = 0
      while row < dense.rows do
        total += dense(row, column)
        row += 1
      assertEqualsDouble(total, 0.0, 1e-12, clues(column))
    }
    raw.sampleKeys.indices.foreach { row =>
      var rowMean = 0.0
      raw.sampleKeys.indices.foreach { column =>
        rowMean += raw.gram.values(row, column)
        val expected =
          raw.gram.values(row, column) -
            centered.statistics.kernelRowMeans(row) -
            centered.statistics.kernelRowMeans(column) +
            centered.statistics.kernelGrandMean
        assertEqualsDouble(
          centered.gram.values(row, column),
          expected,
          0.0,
          clues(row, column)
        )
      }
      assertEqualsDouble(
        centered.gram.values(row, 0) +
          centered.gram.values(row, 1) +
          centered.gram.values(row, 2),
        0.0,
        1e-12,
        clues(row, rowMean)
      )
    }

  test("centered feature dots and operator agree with the centered Gram"):
    val centered = prepared.centered.toOption.get
    val dense = centered.features.values.toDense()

    (0 until dense.rows).foreach { left =>
      (0 until dense.rows).foreach { right =>
        var dot = 0.0
        var column = 0
        while column < dense.cols do
          dot += dense(left, column) * dense(right, column)
          column += 1
        assertEqualsDouble(dot, centered.gram.values(left, right), 1e-12)
      }
    }

    val input = DVec.fromSeq(Vector(0.25, -0.5, 1.25))
    val observed = centered.operator.values(input)
    (0 until centered.graphCount).foreach { row =>
      var expected = 0.0
      var column = 0
      while column < centered.graphCount do
        expected += centered.gram.values(row, column) * input(column)
        column += 1
      assertEqualsDouble(observed(row), expected, 1e-12)
    }
    assertEquals(
      centered.operator.realization,
      KernelOperatorRealization.GramBacked
    )

  test("queries reuse training means and retain query-only norm mass"):
    val raw = prepared
    val rawQuery = raw.cross(query).toOption.get
    val centered = raw.centered.toOption.get
    val observed = centered.cross(query).toOption.get

    assertEquals(
      observed.features.novelSquaredNorm.toSeq,
      rawQuery.features.novelSquaredNorm.toSeq
    )
    assert(observed.features.novelSquaredNorm(0) > 0.0)
    val queryMean =
      (0 until raw.graphCount)
        .map(rawQuery.cross.values(0, _))
        .sum / raw.graphCount.toDouble
    (0 until raw.graphCount).foreach { reference =>
      val expected =
        rawQuery.cross.values(0, reference) -
          queryMean -
          centered.statistics.kernelRowMeans(reference) +
          centered.statistics.kernelGrandMean
      assertEqualsDouble(
        observed.cross.values(0, reference),
        expected,
        0.0
      )
    }
    val fromFeatures =
      centered.features.values(observed.features.shared.row(0))
    (0 until raw.graphCount).foreach { reference =>
      assertEqualsDouble(
        fromFeatures(reference),
        observed.cross.values(0, reference),
        1e-12
      )
    }

  test("centering is immutable across repeated queries"):
    val raw = prepared
    val rawGram = raw.gram.values(0, 1)
    val rawFeatureNnz = raw.features.values.nnz
    val centered = raw.centered.toOption.get
    val meansBefore = centered.statistics.featureMeans.toSeq
    val first = centered.cross(query).toOption.get
    val second = centered.cross(query).toOption.get

    assertEquals(raw.gram.values(0, 1), rawGram)
    assertEquals(raw.features.values.nnz, rawFeatureNnz)
    assertEquals(centered.statistics.featureMeans.toSeq, meansBefore)
    assertEquals(
      java.lang.Double.doubleToLongBits(first.cross.values(0, 0)),
      java.lang.Double.doubleToLongBits(second.cross.values(0, 0))
    )

  test("centering rejects an empty reference corpus"):
    val empty =
      WLCompiler
        .plan(Vector.empty[LabelledNeighbourhood[Int, String, String]], kernel)
        .run()
        .toOption
        .get

    assertEquals(empty.centered, Left(PreparedCenteringError.EmptyReference))

  test("frozen statistics reject cross matrices from another reference"):
    val centered = prepared.centered.toOption.get
    val rawQuery = prepared.cross(query).toOption.get
    val mismatched =
      rawQuery.cross.copy(
        referenceColumns = SampleIndex(Vector(SampleKey("other")))
      )

    assert(
      centered.statistics.centerCross(mismatched).left.exists {
        case PreparedCenteringError.QueryReferenceMismatch(expected, observed) =>
          expected.keys.map(_.value) == centered.features.rows.keys.map(_.value) &&
          observed.keys.map(_.value) == mismatched.referenceColumns.keys.map(_.value)
        case _ => false
      }
    )
