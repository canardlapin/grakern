package grakern.engine

import cats.kernel.Hash
import gale.linalg.{DMat, LinAlgError}
import grakern.core.*
import grakern.standard.shortestpath.*

enum ShortestPathFeatureUnavailable derives CanEqual:
  case PairwiseLoweringSelected

enum ShortestPathProvenanceUnavailable derives CanEqual:
  case PairwiseLoweringSelected

enum CategoricalShortestPathCompileError[V]:
  case APSP(error: ShortestPathAPSPError[V])
  case FeatureMaterialization(error: LinAlgError)
  case Gram(error: GramExecutionFailure)
  case MatrixCapacityExceeded(
      sampleCount: Int,
      requestedEntries: Long
  )
  case PairwiseAccumulationOverflow(
      left: SampleKey,
      right: SampleKey,
      observed: Double
  )
  case DenotationMismatch(
      left: SampleKey,
      right: SampleKey,
      features: Double,
      pairwise: Double,
      tolerance: NonNegativeFinite
  )

final class PreparedCategoricalShortestPathKernel[V, Key] private[engine] (
    val sampleKeys: Vector[SampleKey],
    private val featureSet: Option[
      CategoricalShortestPathFeatureSet[V, Key]
    ],
    val gram: GramMatrix,
    val operator: KernelOperator,
    val capabilities: ShortestPathPreparedCapabilities,
    val planReport: ShortestPathPlanReport
):
  def features: Either[
    ShortestPathFeatureUnavailable,
    FeatureMatrix[
      ShortestPathPart[Key, HopDistance],
      ShortestPathFeatureMetadata[Key]
    ]
  ] =
    featureSet
      .map(_.matrix)
      .toRight(
        ShortestPathFeatureUnavailable.PairwiseLoweringSelected
      )

  def provenance: Either[
    ShortestPathProvenanceUnavailable,
    Vector[ShortestPathFeatureProvenance[V, Key]]
  ] =
    featureSet
      .map(_.provenance)
      .toRight(
        ShortestPathProvenanceUnavailable.PairwiseLoweringSelected
      )

  def kernel(
      leftGraph: Int,
      rightGraph: Int
  ): Option[Double] =
    if leftGraph < 0 || rightGraph < 0 then None
    else if leftGraph >= sampleKeys.size || rightGraph >= sampleKeys.size then None
    else Some(gram.values(leftGraph, rightGraph))

  def explain(
      leftGraph: Int,
      rightGraph: Int
  ): Either[
    ShortestPathProvenanceUnavailable,
    Option[Vector[ShortestPathFeatureContribution[V, Key]]]
  ] =
    featureSet
      .map(_.explain(leftGraph, rightGraph))
      .toRight(
        ShortestPathProvenanceUnavailable.PairwiseLoweringSelected
      )

private object CategoricalShortestPathCompiler:
  def run[V, N, E, Key](
      plan: CategoricalShortestPathPlan[V, N, E, Key]
  ): Either[
    CategoricalShortestPathCompileError[V],
    PreparedCategoricalShortestPathKernel[V, Key]
  ] =
    given Hash[Key] = plan.keyHash
    ShortestPathAPSP
      .prepare(
        plan.samples,
        plan.graph.apspNodeId
      )
      .left
      .map(CategoricalShortestPathCompileError.APSP(_))
      .flatMap { trace =>
        plan.options.lowering match
          case CategoricalShortestPathLowering.ExactFeatures =>
            exact(trace, plan).map { exactResult =>
              prepared(
                trace,
                plan,
                Some(exactResult.features),
                exactResult.gram,
                exactResult.operator,
                ShortestPathNumericalRealization.ExactSparseFeatures,
                None
              )
            }
          case CategoricalShortestPathLowering.Pairwise =>
            pairwise(trace, plan.recipe).map { pairwiseGram =>
              val operator =
                ExactFeatureNumericalViews.operatorFromGram(
                  pairwiseGram
                )
              prepared(
                trace,
                plan,
                None,
                pairwiseGram,
                operator,
                ShortestPathNumericalRealization.ExactPairwise,
                None
              )
            }
          case CategoricalShortestPathLowering.ValidateBoth =>
            for
              exactResult <- exact(trace, plan)
              pairwiseGram <- pairwise(trace, plan.recipe)
              maximumDifference <- compare(
                trace.sampleKeys,
                exactResult.gram,
                pairwiseGram,
                plan.options.validationTolerance
              )
            yield prepared(
              trace,
              plan,
              Some(exactResult.features),
              exactResult.gram,
              exactResult.operator,
              ShortestPathNumericalRealization.DualValidatedFeatures,
              Some(maximumDifference)
            )
      }

  final private case class ExactResult[V, Key](
      features: CategoricalShortestPathFeatureSet[V, Key],
      gram: GramMatrix,
      operator: KernelOperator
  )

  private def exact[V, N, E, Key: Hash](
      trace: UnweightedAPSPTrace[V, N, E],
      plan: CategoricalShortestPathPlan[V, N, E, Key]
  ): Either[
    CategoricalShortestPathCompileError[V],
    ExactResult[V, Key]
  ] =
    CategoricalShortestPathFeatures
      .fromTrace(trace, plan.recipe)
      .left
      .map(
        CategoricalShortestPathCompileError.FeatureMaterialization(
          _
        )
      )
      .flatMap { features =>
        ExactFeatureNumericalViews
          .gram(
            features.matrix,
            features.rows,
            plan.options.gramExecution
          )
          .left
          .map(
            CategoricalShortestPathCompileError.Gram(
              _
            )
          )
          .map { gram =>
            ExactResult(
              features,
              gram,
              ExactFeatureNumericalViews.operator(
                features.matrix
              )
            )
          }
      }

  private def pairwise[V, N, E, Key: Hash](
      trace: UnweightedAPSPTrace[V, N, E],
      recipe: UnweightedShortestPathRecipe[N, E, Key]
  ): Either[
    CategoricalShortestPathCompileError[V],
    GramMatrix
  ] =
    val rows = trace.occurrences(recipe)
    val size = rows.size
    val requestedEntries = size.toLong * size.toLong
    if requestedEntries > Int.MaxValue.toLong then
      Left(
        CategoricalShortestPathCompileError.MatrixCapacityExceeded(
          size,
          requestedEntries
        )
      )
    else
      val values = new Array[Double](requestedEntries.toInt)
      var failure: Option[
        CategoricalShortestPathCompileError[V]
      ] = None
      var leftGraph = 0
      while leftGraph < size && failure.isEmpty do
        var rightGraph = leftGraph
        while rightGraph < size && failure.isEmpty do
          var value = 0.0
          val left = rows(leftGraph)
          val right = rows(rightGraph)
          var leftIndex = 0
          while leftIndex < left.size && failure.isEmpty do
            var rightIndex = 0
            while rightIndex < right.size && failure.isEmpty do
              if CategoricalShortestPathFeatures.sameFeature(
                  left(leftIndex).part,
                  right(rightIndex).part
                )
              then
                val updated = value + 1.0
                if updated.isFinite then value = updated
                else
                  failure = Some(
                    CategoricalShortestPathCompileError
                      .PairwiseAccumulationOverflow(
                        trace.sampleKeys(leftGraph),
                        trace.sampleKeys(rightGraph),
                        updated
                      )
                  )
              rightIndex += 1
            leftIndex += 1
          if failure.isEmpty then
            values(leftGraph * size + rightGraph) = value
            values(rightGraph * size + leftGraph) = value
          rightGraph += 1
        leftGraph += 1
      failure.toLeft(
        GramMatrix(
          toMatrix(size, values),
          SampleIndex(trace.sampleKeys)
        )
      )

  private def compare[V](
      sampleKeys: Vector[SampleKey],
      features: GramMatrix,
      pairwise: GramMatrix,
      tolerance: NonNegativeFinite
  ): Either[
    CategoricalShortestPathCompileError[V],
    Double
  ] =
    var maximumDifference = 0.0
    var failure: Option[
      CategoricalShortestPathCompileError[V]
    ] = None
    var left = 0
    while left < sampleKeys.size && failure.isEmpty do
      var right = 0
      while right < sampleKeys.size && failure.isEmpty do
        val featureValue = features.values(left, right)
        val pairwiseValue = pairwise.values(left, right)
        val difference =
          math.abs(featureValue - pairwiseValue)
        maximumDifference = math.max(maximumDifference, difference)
        if difference > tolerance.toDouble then
          failure = Some(
            CategoricalShortestPathCompileError.DenotationMismatch(
              sampleKeys(left),
              sampleKeys(right),
              featureValue,
              pairwiseValue,
              tolerance
            )
          )
        right += 1
      left += 1
    failure.toLeft(maximumDifference)

  private def prepared[V, N, E, Key](
      trace: UnweightedAPSPTrace[V, N, E],
      plan: CategoricalShortestPathPlan[V, N, E, Key],
      features: Option[
        CategoricalShortestPathFeatureSet[V, Key]
      ],
      gram: GramMatrix,
      operator: KernelOperator,
      realization: ShortestPathNumericalRealization,
      maximumDifference: Option[Double]
  ): PreparedCategoricalShortestPathKernel[V, Key] =
    val hasFeatures = features.isDefined
    val capabilities =
      ShortestPathPreparedCapabilities(
        exactFeatures = hasFeatures,
        gram = true,
        operator = true,
        provenance = hasFeatures,
        cross = false
      )
    val report =
      new ShortestPathPlanReport(
        plan.graph,
        ShortestPathAPSPAlgorithm.UnweightedBfs,
        realization,
        capabilities,
        plan.recipe.fingerprintability,
        trace.sampleKeys.size,
        normalized = false,
        features.map(_.matrix.columns.size),
        features.map(_.matrix.values.nnz),
        operator.realization,
        trace.stats,
        maximumDifference
      )
    new PreparedCategoricalShortestPathKernel(
      trace.sampleKeys,
      features,
      gram,
      operator,
      capabilities,
      report
    )

  private def toMatrix(
      size: Int,
      values: Array[Double]
  ): DMat =
    val builder = DMat.newBuilder(size, size)
    var row = 0
    while row < size do
      var column = 0
      while column < size do
        builder(row, column) = values(row * size + column)
        column += 1
      row += 1
    builder.result()
