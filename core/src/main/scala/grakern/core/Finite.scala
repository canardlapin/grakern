package grakern.core

enum NumericError:
  case NonFinite(value: Double)
  case Negative(value: Double)
  case NonPositive(value: Double)
  case Overflow(operation: String, operands: Vector[Double])

opaque type FiniteDouble = Double

object FiniteDouble:
  def from(value: Double): Either[NumericError, FiniteDouble] =
    if value.isFinite then Right(value)
    else Left(NumericError.NonFinite(value))

  extension (value: FiniteDouble)
    def toDouble: Double = value

    def +(that: FiniteDouble): Either[NumericError, FiniteDouble] =
      checked("addition", value + that, Vector(value, that))

    def *(that: FiniteDouble): Either[NumericError, FiniteDouble] =
      checked("multiplication", value * that, Vector(value, that))

  private def checked(
      operation: String,
      result: Double,
      operands: Vector[Double]
  ): Either[NumericError, FiniteDouble] =
    if result.isFinite then Right(result)
    else Left(NumericError.Overflow(operation, operands))

opaque type NonNegativeFinite = Double

object NonNegativeFinite:
  val zero: NonNegativeFinite = 0.0
  val one: NonNegativeFinite = 1.0

  def from(value: Double): Either[NumericError, NonNegativeFinite] =
    if !value.isFinite then Left(NumericError.NonFinite(value))
    else if value < 0.0 then Left(NumericError.Negative(value))
    else Right(canonicalZero(value))

  extension (value: NonNegativeFinite)
    def toDouble: Double = value

    def +(that: NonNegativeFinite): Either[NumericError, NonNegativeFinite] =
      checked("nonnegative addition", value + that, Vector(value, that))

    def *(that: NonNegativeFinite): Either[NumericError, NonNegativeFinite] =
      checked("nonnegative multiplication", value * that, Vector(value, that))

  private def canonicalZero(value: Double): Double =
    if value == 0.0 then 0.0 else value

  private def checked(
      operation: String,
      result: Double,
      operands: Vector[Double]
  ): Either[NumericError, NonNegativeFinite] =
    if result.isFinite then Right(canonicalZero(result))
    else Left(NumericError.Overflow(operation, operands))

opaque type PositiveFinite = Double

object PositiveFinite:
  val one: PositiveFinite = 1.0

  def from(value: Double): Either[NumericError, PositiveFinite] =
    if !value.isFinite then Left(NumericError.NonFinite(value))
    else if value <= 0.0 then Left(NumericError.NonPositive(value))
    else Right(value)

  extension (value: PositiveFinite)
    def toDouble: Double = value

    def +(that: PositiveFinite): Either[NumericError, PositiveFinite] =
      checked("positive addition", value + that, Vector(value, that))

    def *(that: PositiveFinite): Either[NumericError, PositiveFinite] =
      checked("positive multiplication", value * that, Vector(value, that))

  private def checked(
      operation: String,
      result: Double,
      operands: Vector[Double]
  ): Either[NumericError, PositiveFinite] =
    if !result.isFinite then Left(NumericError.Overflow(operation, operands))
    else if result <= 0.0 then Left(NumericError.NonPositive(result))
    else Right(result)
