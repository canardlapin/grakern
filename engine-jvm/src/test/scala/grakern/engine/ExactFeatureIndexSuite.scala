package grakern.engine

import grakern.core.SampleKey
import munit.FunSuite

final class ExactFeatureIndexSuite extends FunSuite:
  final private case class CollisionKey(
      semanticId: Int,
      witness: String
  )

  private val sameKey =
    (left: CollisionKey, right: CollisionKey) => left.semanticId == right.semanticId

  private val constantHash =
    (_: CollisionKey) => 7

  test("collision buckets retain exact identity and first-seen column order"):
    val first =
      CollisionKey(1, "first")
    val equivalent =
      CollisionKey(1, "equivalent")
    val second =
      CollisionKey(2, "second")
    val third =
      CollisionKey(3, "third")
    val rows =
      Vector(
        ExactFeatureRow(
          Vector(first -> 2.0, second -> 3.0)
        ),
        ExactFeatureRow(
          Vector(equivalent -> 5.0, third -> 7.0)
        )
      )
    val features =
      ExactFeatureMatrix
        .materialize(
          Vector(SampleKey("a"), SampleKey("b")),
          rows,
          constantHash,
          sameKey,
          identity
        )
        .toOption
        .get

    assertEquals(
      features.columns.coordinates,
      Vector(first, second, third)
    )
    assertEquals(
      features.columns.indexOf(equivalent),
      Some(0)
    )

    val gram =
      ExactFeatureNumericalViews
        .gram(
          features,
          rows,
          constantHash,
          sameKey,
          GramExecution.Sequential
        )
        .toOption
        .get
        .values

    assertEquals(gram(0, 0), 13.0)
    assertEquals(gram(0, 1), 10.0)
    assertEquals(gram(1, 0), 10.0)
    assertEquals(gram(1, 1), 74.0)
