package grakern.engine

import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinearOperator, MutableDVec}

final case class GramMatrix(
    values: DMat,
    samples: SampleIndex
)

enum KernelOperatorRealization derives CanEqual:
  case FeatureBacked

final case class KernelOperator(
    values: DoubleLinearOperator,
    samples: SampleIndex,
    realization: KernelOperatorRealization
)

private[grakern] object WLNumericalViews:
  def gram(
      features: FeatureMatrix,
      rows: Vector[ExactFeatureRow[WLFeatureCoordinate]]
  ): GramMatrix =
    val size = rows.size
    val builder = DMat.newBuilder(size, size)
    var left = 0
    while left < size do
      var right = left
      while right < size do
        val value =
          rows(left).dot(rows(right))(
            WLCompiler.sameCoordinate
          )
        builder(left, right) = value
        if left != right then builder(right, left) = value
        right += 1
      left += 1
    GramMatrix(builder.result(), features.rows)

  def operator(features: FeatureMatrix): KernelOperator =
    val values = features.values
    val operator =
      LinearOperator.fromFunction(values.rows, values.rows) { (input: DVec, output: MutableDVec) =>
        val featureWeights = MutableDVec.zeros(values.cols)
        values.transposeApplyTo(input, featureWeights)
        values.applyTo(featureWeights.toVec, output)
      }
    KernelOperator(
      operator,
      features.rows,
      KernelOperatorRealization.FeatureBacked
    )
