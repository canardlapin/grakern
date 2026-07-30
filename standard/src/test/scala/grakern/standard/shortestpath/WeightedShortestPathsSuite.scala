package grakern.standard.shortestpath

import grakern.core.*
import munit.FunSuite

final class WeightedShortestPathsSuite extends FunSuite:
  final case class Atom(element: String)
  final case class Bond(length: Double)

  test("path lengths reject negative and nonfinite values"):
    assertEquals(
      PathLength.from(-0.5),
      Left(NumericError.Negative(-0.5))
    )
    assert(
      PathLength
        .from(Double.NaN)
        .left
        .exists(
          _.isInstanceOf[NumericError.NonFinite]
        )
    )
    assert(
      PathLength
        .from(Double.PositiveInfinity)
        .left
        .exists(
          _.isInstanceOf[NumericError.NonFinite]
        )
    )

  test("path lengths accept zero and canonicalize negative zero"):
    assertEquals(PathLength.from(0.0).map(_.toDouble), Right(0.0))
    assertEquals(
      PathLength.from(-0.0).map(value => java.lang.Double.doubleToRawLongBits(value.toDouble)),
      Right(java.lang.Double.doubleToRawLongBits(0.0))
    )

  test("reusing edge-length semantics preserves its plan identity"):
    val lengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.of[Bond, Double](_.length)
      )
    val first =
      WeightedShortestPaths.parts(
        Projection.of[Atom, String](_.element),
        lengths
      )
    val second =
      WeightedShortestPaths.parts(
        Projection.of[Atom, Int](_.element.length),
        lengths
      )

    assertEquals(
      first.edgeLengths.planNodeId.number,
      second.edgeLengths.planNodeId.number
    )
    assert(
      first.planNodeId.number != second.planNodeId.number
    )

  test("named projections make weighted semantics durable"):
    val lengths =
      WeightedEdgeLengths(
        ProjectionDescriptor("bond.length"),
        Projection.named[Bond, Double](
          StableId.from("molecule.bond.length").toOption.get,
          StableVersion.initial
        )(_.length)
      )
    val recipe =
      WeightedShortestPaths.parts(
        Projection.named[Atom, String](
          StableId.from("molecule.atom.element").toOption.get,
          StableVersion.initial
        )(_.element),
        lengths
      )

    assert(
      recipe.fingerprintability
        .isInstanceOf[Fingerprintability.Durable]
    )
