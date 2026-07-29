package grakern.engine

import grakern.core.*

private[grakern] enum ExactFeatureNormalizationError:
  case NonFiniteDiagonal(
      sample: SampleKey,
      index: Int,
      value: Double
  )
  case NegativeDiagonal(
      sample: SampleKey,
      index: Int,
      value: Double,
      tolerance: NonNegativeFinite
  )

private[grakern] object ExactFeatureRows:
  def normalize[K](
      sampleKeys: Vector[SampleKey],
      rows: Vector[ExactFeatureRow[K]],
      tolerance: NonNegativeFinite
  ): Either[
    ExactFeatureNormalizationError,
    (
        Vector[ExactFeatureRow[K]],
        Vector[NumericalAuditEvent]
    )
  ] =
    val normalized = Vector.newBuilder[ExactFeatureRow[K]]
    val events = Vector.newBuilder[NumericalAuditEvent]
    var failure: Option[ExactFeatureNormalizationError] = None
    var index = 0

    while index < rows.size && failure.isEmpty do
      val diagonal = rows(index).squaredNorm
      if !diagonal.isFinite then
        failure = Some(
          ExactFeatureNormalizationError.NonFiniteDiagonal(
            sampleKeys(index),
            index,
            diagonal
          )
        )
      else if diagonal < -tolerance.toDouble then
        failure = Some(
          ExactFeatureNormalizationError.NegativeDiagonal(
            sampleKeys(index),
            index,
            diagonal,
            tolerance
          )
        )
      else if math.abs(diagonal) <= tolerance.toDouble then
        normalized += ExactFeatureRow(Vector.empty)
        events += NumericalAuditEvent.DiagonalTreatedAsZero(
          sampleKeys(index),
          index,
          diagonal,
          tolerance
        )
      else
        normalized += rows(index).scaled(
          1.0 / math.sqrt(diagonal)
        )
      index += 1

    failure.toLeft(normalized.result() -> events.result())
