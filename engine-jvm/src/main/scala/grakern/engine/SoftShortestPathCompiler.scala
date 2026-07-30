package grakern.engine

import gale.linalg.DMat
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*

enum SoftPartEvaluationStage derives CanEqual:
  case EndpointKernel
  case DirectEndpointProduct
  case SwappedEndpointProduct
  case EndpointSum
  case DistanceKernel
  case PartProduct
  case ConvolutionSum

final case class SoftShortestPathNumericFailure[V](
    leftSample: SampleKey,
    rightSample: SampleKey,
    leftWitness: ShortestPathWitness[V],
    rightWitness: ShortestPathWitness[V],
    stage: SoftPartEvaluationStage,
    reason: NumericError
)

enum SoftShortestPathCompileError[V]:
  case Dijkstra(error: WeightedShortestPathAPSPError[V])
  case Trace(error: WeightedAPSPTraceError)
  case Numeric(error: SoftShortestPathNumericFailure[V])
  case Normalization(
      error: PrepareError[Nothing, Nothing]
  )
  case MatrixCapacityExceeded(
      sampleCount: Int,
      requestedEntries: Long
  )

final case class SoftShortestPathExecutionStats(
    apsp: WeightedShortestPathAPSPStats,
    graphPairEvaluations: Long,
    partPairEvaluations: Long
)

final private[grakern] class CompiledSoftShortestPathKernel[V] private[engine] (
    val sampleKeys: Vector[SampleKey],
    val gram: GramMatrix,
    val semanticFingerprint: Fingerprintability,
    val normalized: Boolean,
    val auditEvents: Vector[NumericalAuditEvent],
    val executionStats: SoftShortestPathExecutionStats
):
  def kernel(
      leftGraph: Int,
      rightGraph: Int
  ): Option[Double] =
    if leftGraph < 0 || rightGraph < 0 then None
    else if leftGraph >= sampleKeys.size || rightGraph >= sampleKeys.size then None
    else Some(gram.values(leftGraph, rightGraph))

object SoftShortestPathCompiler:
  final private case class Semantic[N, E, Key](
      base: SoftShortestPathKernelExpr.Convolution[N, E, Key],
      normalized: Boolean
  )

  def prepare[V, N, E, Key](
      samples: Vector[GraphSample[V, N, E]],
      kernel: SoftShortestPathKernel[N, E, Key],
      diagonalTolerance: NonNegativeFinite = NonNegativeFinite.zero
  ): Either[
    SoftShortestPathCompileError[V],
    CompiledSoftShortestPathKernel[V]
  ] =
    val selected = semantic(kernel.expression)
    WeightedShortestPathAPSP
      .prepare(
        samples,
        selected.base.recipe.edgeLengths
      )
      .left
      .map(SoftShortestPathCompileError.Dijkstra(_))
      .flatMap { trace =>
        trace
          .occurrences(selected.base.recipe)
          .left
          .map(SoftShortestPathCompileError.Trace(_))
          .flatMap { rows =>
            evaluate(
              trace.sampleKeys,
              rows,
              selected.base.endpointKernel,
              selected.base.distanceKernel
            ).flatMap {
              case (
                    raw,
                    graphPairEvaluations,
                    partPairEvaluations
                  ) =>
                val normalized: Either[
                  SoftShortestPathCompileError[V],
                  (
                      Array[Double],
                      Vector[NumericalAuditEvent]
                  )
                ] =
                  if selected.normalized then
                    normalize(
                      trace.sampleKeys,
                      raw,
                      diagonalTolerance
                    ).left.map(error =>
                      SoftShortestPathCompileError.Normalization(
                        error
                      )
                    )
                  else Right(raw -> Vector.empty)
                normalized.map { case (values, events) =>
                  new CompiledSoftShortestPathKernel(
                    trace.sampleKeys,
                    GramMatrix(
                      toMatrix(trace.sampleKeys.size, values),
                      SampleIndex(trace.sampleKeys)
                    ),
                    kernel.fingerprintability,
                    selected.normalized,
                    events,
                    SoftShortestPathExecutionStats(
                      trace.stats,
                      graphPairEvaluations,
                      partPairEvaluations
                    )
                  )
                }
            }
          }
      }

  private def semantic[N, E, Key](
      expression: SoftShortestPathKernelExpr[N, E, Key]
  ): Semantic[N, E, Key] =
    expression match
      case base: SoftShortestPathKernelExpr.Convolution[N, E, Key] =>
        Semantic(base, normalized = false)
      case normalized: SoftShortestPathKernelExpr.Normalize[N, E, Key] =>
        semantic(normalized.child).copy(normalized = true)

  private def evaluate[V, Key](
      sampleKeys: Vector[SampleKey],
      rows: Vector[
        Vector[ShortestPathOccurrence[V, Key, PathLength]]
      ],
      endpointKernel: Kernel[Key],
      distanceKernel: Kernel[Double]
  ): Either[
    SoftShortestPathCompileError[V],
    (Array[Double], Long, Long)
  ] =
    val size = rows.size
    val requestedEntries = size.toLong * size.toLong
    val capacityFailure: Option[
      SoftShortestPathCompileError[V]
    ] =
      if requestedEntries > Int.MaxValue.toLong then
        Some(
          SoftShortestPathCompileError.MatrixCapacityExceeded(
            size,
            requestedEntries
          )
        )
      else None
    val cells =
      if capacityFailure.isEmpty then new Array[Double](requestedEntries.toInt)
      else Array.emptyDoubleArray
    var graphPairEvaluations = 0L
    var partPairEvaluations = 0L
    var failure: Option[SoftShortestPathCompileError[V]] =
      capacityFailure
    var leftGraph = 0
    while leftGraph < size && failure.isEmpty do
      var rightGraph = leftGraph
      while rightGraph < size && failure.isEmpty do
        graphPairEvaluations += 1L
        convolution(
          sampleKeys(leftGraph),
          sampleKeys(rightGraph),
          rows(leftGraph),
          rows(rightGraph),
          endpointKernel,
          distanceKernel
        ) match
          case Right((value, comparisons)) =>
            cells(leftGraph * size + rightGraph) = value
            cells(rightGraph * size + leftGraph) = value
            partPairEvaluations += comparisons
          case Left(error) =>
            failure = Some(
              SoftShortestPathCompileError.Numeric(error)
            )
        rightGraph += 1
      leftGraph += 1
    failure.toLeft(
      (
        cells,
        graphPairEvaluations,
        partPairEvaluations
      )
    )

  private def convolution[V, Key](
      leftSample: SampleKey,
      rightSample: SampleKey,
      left: Vector[
        ShortestPathOccurrence[V, Key, PathLength]
      ],
      right: Vector[
        ShortestPathOccurrence[V, Key, PathLength]
      ],
      endpointKernel: Kernel[Key],
      distanceKernel: Kernel[Double]
  ): Either[
    SoftShortestPathNumericFailure[V],
    (Double, Long)
  ] =
    var result = 0.0
    var comparisons = 0L
    var failure: Option[
      SoftShortestPathNumericFailure[V]
    ] = None
    var leftIndex = 0
    while leftIndex < left.size && failure.isEmpty do
      var rightIndex = 0
      while rightIndex < right.size && failure.isEmpty do
        val leftPart = left(leftIndex)
        val rightPart = right(rightIndex)
        comparisons += 1L
        partValue(
          leftSample,
          rightSample,
          leftPart,
          rightPart,
          endpointKernel,
          distanceKernel
        ) match
          case Right(value) =>
            checkedAddition(
              result,
              value,
              leftSample,
              rightSample,
              leftPart.witness,
              rightPart.witness,
              SoftPartEvaluationStage.ConvolutionSum
            ) match
              case Right(updated) => result = updated
              case Left(error)    => failure = Some(error)
          case Left(error) =>
            failure = Some(error)
        rightIndex += 1
      leftIndex += 1
    failure.toLeft(result -> comparisons)

  private def partValue[V, Key](
      leftSample: SampleKey,
      rightSample: SampleKey,
      left: ShortestPathOccurrence[V, Key, PathLength],
      right: ShortestPathOccurrence[V, Key, PathLength],
      endpointKernel: Kernel[Key],
      distanceKernel: Kernel[Double]
  ): Either[SoftShortestPathNumericFailure[V], Double] =
    val leftEndpoints = left.part.endpoints
    val rightEndpoints = right.part.endpoints

    for
      firstFirst <- checkedValue(
        endpointKernel.evaluate(
          leftEndpoints.first,
          rightEndpoints.first
        ),
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.EndpointKernel
      )
      secondSecond <- checkedValue(
        endpointKernel.evaluate(
          leftEndpoints.second,
          rightEndpoints.second
        ),
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.EndpointKernel
      )
      direct <- checkedProduct(
        firstFirst,
        secondSecond,
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.DirectEndpointProduct
      )
      firstSecond <- checkedValue(
        endpointKernel.evaluate(
          leftEndpoints.first,
          rightEndpoints.second
        ),
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.EndpointKernel
      )
      secondFirst <- checkedValue(
        endpointKernel.evaluate(
          leftEndpoints.second,
          rightEndpoints.first
        ),
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.EndpointKernel
      )
      swapped <- checkedProduct(
        firstSecond,
        secondFirst,
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.SwappedEndpointProduct
      )
      endpoint <- checkedAddition(
        direct,
        swapped,
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.EndpointSum
      )
      distance <- checkedValue(
        distanceKernel.evaluate(
          left.part.distance.toDouble,
          right.part.distance.toDouble
        ),
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.DistanceKernel
      )
      result <- checkedProduct(
        endpoint,
        distance,
        leftSample,
        rightSample,
        left.witness,
        right.witness,
        SoftPartEvaluationStage.PartProduct
      )
    yield result

  private def checkedValue[V](
      value: Double,
      leftSample: SampleKey,
      rightSample: SampleKey,
      leftWitness: ShortestPathWitness[V],
      rightWitness: ShortestPathWitness[V],
      stage: SoftPartEvaluationStage
  ): Either[SoftShortestPathNumericFailure[V], Double] =
    FiniteDouble
      .from(value)
      .left
      .map(
        SoftShortestPathNumericFailure(
          leftSample,
          rightSample,
          leftWitness,
          rightWitness,
          stage,
          _
        )
      )
      .map(_.toDouble)

  private def checkedProduct[V](
      left: Double,
      right: Double,
      leftSample: SampleKey,
      rightSample: SampleKey,
      leftWitness: ShortestPathWitness[V],
      rightWitness: ShortestPathWitness[V],
      stage: SoftPartEvaluationStage
  ): Either[SoftShortestPathNumericFailure[V], Double] =
    val evaluated =
      for
        finiteLeft <- FiniteDouble.from(left)
        finiteRight <- FiniteDouble.from(right)
        result <- finiteLeft * finiteRight
      yield result.toDouble
    evaluated.left.map(
      SoftShortestPathNumericFailure(
        leftSample,
        rightSample,
        leftWitness,
        rightWitness,
        stage,
        _
      )
    )

  private def checkedAddition[V](
      left: Double,
      right: Double,
      leftSample: SampleKey,
      rightSample: SampleKey,
      leftWitness: ShortestPathWitness[V],
      rightWitness: ShortestPathWitness[V],
      stage: SoftPartEvaluationStage
  ): Either[SoftShortestPathNumericFailure[V], Double] =
    val evaluated =
      for
        finiteLeft <- FiniteDouble.from(left)
        finiteRight <- FiniteDouble.from(right)
        result <- finiteLeft + finiteRight
      yield result.toDouble
    evaluated.left.map(
      SoftShortestPathNumericFailure(
        leftSample,
        rightSample,
        leftWitness,
        rightWitness,
        stage,
        _
      )
    )

  private def normalize(
      sampleKeys: Vector[SampleKey],
      values: Array[Double],
      tolerance: NonNegativeFinite
  ): Either[
    PrepareError[Nothing, Nothing],
    (Array[Double], Vector[NumericalAuditEvent])
  ] =
    val size = sampleKeys.size
    val norms = new Array[Double](size)
    val events = Vector.newBuilder[NumericalAuditEvent]
    var failure: Option[
      PrepareError[Nothing, Nothing]
    ] = None
    var index = 0
    while index < size && failure.isEmpty do
      val diagonal = values(index * size + index)
      if diagonal < -tolerance.toDouble then
        failure = Some(
          PrepareError.InvalidNegativeDiagonal(
            sampleKeys(index),
            index,
            diagonal,
            tolerance
          )
        )
      else if math.abs(diagonal) <= tolerance.toDouble then
        norms(index) = 0.0
        events += NumericalAuditEvent.DiagonalTreatedAsZero(
          sampleKeys(index),
          index,
          diagonal,
          tolerance
        )
      else norms(index) = math.sqrt(diagonal)
      index += 1

    failure match
      case Some(error) =>
        Left(error)
      case None =>
        val result = new Array[Double](values.length)
        var left = 0
        while left < size && failure.isEmpty do
          var right = left
          while right < size && failure.isEmpty do
            val normalized =
              if norms(left) == 0.0 || norms(right) == 0.0 then 0.0
              else
                values(left * size + right) /
                  norms(left) /
                  norms(right)
            if !normalized.isFinite ||
              math.abs(normalized) >
                1.0 + tolerance.toDouble
            then
              failure = Some(
                PrepareError.InvalidNormalizedValue(
                  sampleKeys(left),
                  sampleKeys(right),
                  normalized,
                  tolerance
                )
              )
            else
              result(left * size + right) = normalized
              result(right * size + left) = normalized
            right += 1
          left += 1
        failure.toLeft(result -> events.result())

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
