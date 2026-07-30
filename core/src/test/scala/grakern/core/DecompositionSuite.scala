package grakern.core

import munit.FunSuite

final class DecompositionSuite extends FunSuite:
  private val source =
    Decomposition.fromWeighted[Vector[Int], Int](
      _.map(value => value -> NonNegativeFinite.one)
    )

  private def materialize[A, P](
      decomposition: Decomposition[A, P],
      value: A
  ): Either[
    DecompositionError,
    Vector[(P, Double)]
  ] =
    decomposition.fold(value, Vector.empty) { (result, pattern, weight) =>
      result :+ (pattern -> weight.toDouble)
    }

  test("fold agrees with the materialized counting measure"):
    assertEquals(
      materialize(source, Vector(1, 2, 2)),
      Right(Vector(1 -> 1.0, 2 -> 1.0, 2 -> 1.0))
    )

  test("map obeys identity and composition"):
    val values = Vector(1, 2, 3)
    val identityMapped =
      materialize(source.map(identity), values)
    val composed =
      materialize(
        source.map(_ + 1).map(_.toString),
        values
      )
    val direct =
      materialize(
        source.map(value => (value + 1).toString),
        values
      )

    assertEquals(identityMapped, materialize(source, values))
    assertEquals(composed, direct)

  test("flatMap multiplies weights and remains fused"):
    val half =
      NonNegativeFinite.from(0.5).toOption.get
    val expanded =
      source.flatMap { pattern =>
        Decomposition.fromWeighted[Unit, String] { _ =>
          Vector(
            s"$pattern-left" -> half,
            s"$pattern-right" -> half
          )
        }
      }

    assertEquals(
      materialize(expanded, Vector(1)),
      Right(
        Vector(
          "1-left" -> 0.5,
          "1-right" -> 0.5
        )
      )
    )

  test("composition reports finite-weight overflow"):
    val large =
      NonNegativeFinite
        .from(Double.MaxValue)
        .toOption
        .get
    val overflowing =
      Decomposition
        .single("pattern", large)
        .reweight(_ => large)

    materialize(overflowing, ()) match
      case Left(
            DecompositionError.Numeric(
              NumericError.Overflow(_, _)
            )
          ) =>
        ()
      case other =>
        fail(s"expected checked overflow, observed $other")
