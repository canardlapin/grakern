package grakern.core

import munit.FunSuite

final class FiniteSuite extends FunSuite:
  test("finite values reject NaN and infinities"):
    assert(FiniteDouble.from(Double.NaN).isLeft)
    assert(FiniteDouble.from(Double.PositiveInfinity).isLeft)
    assert(FiniteDouble.from(Double.NegativeInfinity).isLeft)
    assertEquals(FiniteDouble.from(2.5).map(_.toDouble), Right(2.5))

  test("nonnegative values reject negatives and canonicalize negative zero"):
    assert(NonNegativeFinite.from(-1.0).isLeft)
    val negativeZero = NonNegativeFinite.from(-0.0).map(_.toDouble)
    assertEquals(negativeZero, Right(0.0))
    assertEquals(
      negativeZero.map(java.lang.Double.doubleToRawLongBits),
      Right(java.lang.Double.doubleToRawLongBits(0.0))
    )

  test("positive values reject zero and negative zero"):
    assert(PositiveFinite.from(0.0).isLeft)
    assert(PositiveFinite.from(-0.0).isLeft)
    assertEquals(PositiveFinite.from(0.25).map(_.toDouble), Right(0.25))

  test("checked arithmetic reports overflow"):
    val maximum = FiniteDouble.from(Double.MaxValue).toOption.get
    val two = FiniteDouble.from(2.0).toOption.get
    assertEquals(
      maximum * two,
      Left(
        NumericError.Overflow(
          "multiplication",
          Vector(Double.MaxValue, 2.0)
        )
      )
    )

  test("nonnegative arithmetic retains its validated result type"):
    val two = NonNegativeFinite.from(2.0).toOption.get
    val three = NonNegativeFinite.from(3.0).toOption.get
    assertEquals((two + three).map(_.toDouble), Right(5.0))
    assertEquals((two * three).map(_.toDouble), Right(6.0))

  test("typed preparation errors retain source and projection"):
    val source = SourceRef.Node(SampleKey("molecule-42"), 17)
    val error =
      PrepareError.InvalidNumeric(
        source,
        ProjectionDescriptor("atom.charge"),
        Double.NaN,
        NumericError.NonFinite(Double.NaN)
      )
    assertEquals(error.source, source)
    assertEquals(error.projection.value, "atom.charge")
