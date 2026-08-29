package grakern.engine

import cats.kernel.Hash
import gale.linalg.LinAlgError
import grakern.core.*
import grakern.standard.wl.*

enum WLCompiledFeature:
  case Subtree(feature: WLSubtreeFeature)
  case OptimalAssignment(
      feature: HierarchyFeature[WLHierarchyNode]
  )

final case class WLFeatureCoordinate(
    blockPath: Vector[Int],
    feature: WLCompiledFeature
)

final case class WLExecutionStats(
    refinementExecutions: Int,
    subtreeExtractions: Int,
    optimalAssignmentExtractions: Int
)

sealed trait WLCompileError[+V]

object WLCompileError:
  final case class Trace[V](error: WLTraceError[V]) extends WLCompileError[V]

  final case class Subtree(error: WLSubtreeFeatureError) extends WLCompileError[Nothing]

  final case class Hierarchy(
      error: HierarchyError[WLHierarchyNode, WLColour]
  ) extends WLCompileError[Nothing]

  final case class Normalization(
      error: ExactFeatureNormalizationError
  ) extends WLCompileError[Nothing]

  final case class PortableFeatures(
      error: WLFeatureEmissionError[Nothing]
  ) extends WLCompileError[Nothing]

  final case class MissingPortableCoordinateOrigin(
      column: Int
  ) extends WLCompileError[Nothing]

  final case class FeatureMaterialization(error: LinAlgError) extends WLCompileError[Nothing]

  final case class Gram(error: GramExecutionFailure) extends WLCompileError[Nothing]

  final case class RequestedOutput(
      expected: WLRequestedOutput,
      actual: WLRequestedOutput
  ) extends WLCompileError[Nothing]

private[engine] trait WLQueryTraining[V, N, E, NodeKey, EdgeKey]:
  private[engine] def expression: WLKernelExpr[N, E, NodeKey, EdgeKey]
  private[engine] def diagonalTolerance: NonNegativeFinite
  def features: FeatureMatrix[WLFeatureCoordinate, WLFeatureMetadata]
  def strategy: WLExecutionStrategy
  private[engine] def trainingTrace(
      refinementNodeId: PlanNodeId
  ): Option[WLTrace[V, NodeKey, EdgeKey]]

final class WLCompiledKernel[V, N, E, NodeKey, EdgeKey] private[engine] (
    val sampleKeys: Vector[SampleKey],
    private[grakern] val rows: Vector[PortableSparseFeatureRow],
    private[engine] val traces: Vector[
      (
          WLRefinement[N, E, NodeKey, EdgeKey],
          WLTrace[V, NodeKey, EdgeKey]
      )
    ],
    override private[engine] val expression: WLKernelExpr[
      N,
      E,
      NodeKey,
      EdgeKey
    ],
    override private[engine] val diagonalTolerance: NonNegativeFinite,
    val planGraph: WLPlanGraph,
    val semanticFingerprint: Fingerprintability,
    val artifactFingerprint: ArtifactFingerprintability,
    override val features: FeatureMatrix[
      WLFeatureCoordinate,
      WLFeatureMetadata
    ],
    val gram: GramMatrix,
    val operator: KernelOperator,
    val capabilities: WLPreparedCapabilities,
    override val strategy: WLExecutionStrategy,
    val planReport: WLPlanReport,
    val auditEvents: Vector[NumericalAuditEvent],
    val executionStats: WLExecutionStats
) extends WLQueryTraining[V, N, E, NodeKey, EdgeKey]:
  def graphCount: Int = sampleKeys.size

  def activeCoordinates(graphIndex: Int): Option[Int] =
    rows.lift(graphIndex).map(_.activeSize)

  def squaredNorm(graphIndex: Int): Option[Double] =
    rows.lift(graphIndex).map(_.squaredNorm)

  def kernel(leftGraph: Int, rightGraph: Int): Option[Double] =
    for
      left <- rows.lift(leftGraph)
      right <- rows.lift(rightGraph)
    yield left.dot(right)

  def cross[QueryV](
      samples: Vector[LabelledNeighbourhood[QueryV, N, E]]
  ): Either[WLQueryCompileError[QueryV], WLQueryResult] =
    WLQueryAlignment.prepare(this, samples)

  /** Align and evaluate one query without a dense feature or cross-vector allocation. */
  def query[QueryV](
      sample: LabelledNeighbourhood[QueryV, N, E]
  ): Either[WLQueryCompileError[QueryV], WLQueryRowResult] =
    WLQueryAlignment.prepareSingle(this, sample)

  /** Center every numerical view against this immutable training corpus.
    *
    * Centering is intentionally a prepared operation: the returned value freezes training means and
    * reuses them for query cross matrices. It is not exposed as a context-free `WLKernel`.
    */
  def centered: Either[
    PreparedCenteringError,
    CenteredWLCompiledKernel[V, N, E, NodeKey, EdgeKey]
  ] =
    PreparedCentering.wl(this)

  override private[engine] def trainingTrace(
      refinementNodeId: PlanNodeId
  ): Option[WLTrace[V, NodeKey, EdgeKey]] =
    traces
      .find(_._1.planNodeId.number == refinementNodeId.number)
      .map(_._2)

/** Training state compiled specifically for query alignment.
  *
  * Unlike [[WLCompiledKernel]], this preparation does not allocate a training Gram matrix or
  * operator. It is the explicit path for callers that requested only query rows or cross values.
  */
final class WLPreparedQueryKernel[V, N, E, NodeKey, EdgeKey] private[engine] (
    val sampleKeys: Vector[SampleKey],
    private[grakern] val rows: Vector[PortableSparseFeatureRow],
    private[engine] val traces: Vector[
      (
          WLRefinement[N, E, NodeKey, EdgeKey],
          WLTrace[V, NodeKey, EdgeKey]
      )
    ],
    override private[engine] val expression: WLKernelExpr[N, E, NodeKey, EdgeKey],
    override private[engine] val diagonalTolerance: NonNegativeFinite,
    val planGraph: WLPlanGraph,
    val semanticFingerprint: Fingerprintability,
    val artifactFingerprint: ArtifactFingerprintability,
    override val features: FeatureMatrix[WLFeatureCoordinate, WLFeatureMetadata],
    override val strategy: WLExecutionStrategy,
    val auditEvents: Vector[NumericalAuditEvent],
    val executionStats: WLExecutionStats
) extends WLQueryTraining[V, N, E, NodeKey, EdgeKey]:
  def graphCount: Int = sampleKeys.size

  def cross[QueryV](
      samples: Vector[LabelledNeighbourhood[QueryV, N, E]]
  ): Either[WLQueryCompileError[QueryV], WLQueryResult] =
    WLQueryAlignment.prepare(this, samples)

  def query[QueryV](
      sample: LabelledNeighbourhood[QueryV, N, E]
  ): Either[WLQueryCompileError[QueryV], WLQueryRowResult] =
    WLQueryAlignment.prepareSingle(this, sample)

  override private[engine] def trainingTrace(
      refinementNodeId: PlanNodeId
  ): Option[WLTrace[V, NodeKey, EdgeKey]] =
    traces
      .find(_._1.planNodeId.number == refinementNodeId.number)
      .map(_._2)

object WLCompiler:
  def plan[
      V,
      N,
      E,
      NodeKey: Hash,
      EdgeKey: Hash
  ](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      kernel: WLKernel[N, E, NodeKey, EdgeKey],
      options: WLPlanOptions = WLPlanOptions()
  ): WLPlan[V, N, E, NodeKey, EdgeKey] =
    WLPlan.create(samples, kernel, options)

  /** Plan a feature basis for later `query` or `cross` calls. */
  def queryPlan[
      V,
      N,
      E,
      NodeKey: Hash,
      EdgeKey: Hash
  ](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      kernel: WLKernel[N, E, NodeKey, EdgeKey],
      options: WLPlanOptions = WLPlanOptions()
  ): WLPlan[V, N, E, NodeKey, EdgeKey] =
    WLPlan.create(
      samples,
      kernel,
      options.copy(requestedOutput = WLRequestedOutput.Query)
    )

  final private case class FeaturePreparation[V, N, E, NodeKey, EdgeKey](
      emission: WLFeatureEmission,
      traces: Vector[
        (
            WLRefinement[N, E, NodeKey, EdgeKey],
            WLTrace[V, NodeKey, EdgeKey]
        )
      ],
      features: FeatureMatrix[WLFeatureCoordinate, WLFeatureMetadata],
      strategy: WLExecutionStrategy,
      stats: WLExecutionStats
  )

  private[engine] def run[
      V,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      plan: WLPlan[V, N, E, NodeKey, EdgeKey]
  ): Either[
    WLCompileError[V],
    WLCompiledKernel[V, N, E, NodeKey, EdgeKey]
  ] =
    prepareFeatures(plan).flatMap { prepared =>
      ExactFeatureNumericalViews
        .gramPortable(
          prepared.features,
          prepared.emission.rows,
          plan.options.gramExecution,
          prepared.strategy.gram
        )
        .left
        .map(WLCompileError.Gram(_))
        .map { gram =>
          val operator =
            ExactFeatureNumericalViews.operator(prepared.features)
          val capabilities =
            WLPreparedCapabilities(
              exactFeatures = true,
              gram = true,
              cross = true,
              operator = true,
              queryAlignment = true,
              provenance = true
            )
          val report =
            WLPlanReport.from(
              plan.graph,
              plan.semanticFingerprint,
              plan.artifactFingerprint,
              prepared.features,
              operator,
              plan.options.gramExecution,
              prepared.strategy,
              prepared.stats
            )
          new WLCompiledKernel(
            plan.samples.map(_.key),
            prepared.emission.rows,
            prepared.traces,
            plan.kernel.expression,
            plan.options.diagonalTolerance,
            plan.graph,
            plan.semanticFingerprint,
            plan.artifactFingerprint,
            prepared.features,
            gram,
            operator,
            capabilities,
            prepared.strategy,
            report,
            prepared.emission.auditEvents,
            prepared.stats
          )
        }
    }

  private[engine] def runForQueries[
      V,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      plan: WLPlan[V, N, E, NodeKey, EdgeKey]
  ): Either[
    WLCompileError[V],
    WLPreparedQueryKernel[V, N, E, NodeKey, EdgeKey]
  ] =
    if plan.options.requestedOutput != WLRequestedOutput.Query then
      Left(
        WLCompileError.RequestedOutput(
          WLRequestedOutput.Query,
          plan.options.requestedOutput
        )
      )
    else
      prepareFeatures(plan).map { prepared =>
        new WLPreparedQueryKernel(
          plan.samples.map(_.key),
          prepared.emission.rows,
          prepared.traces,
          plan.kernel.expression,
          plan.options.diagonalTolerance,
          plan.graph,
          plan.semanticFingerprint,
          plan.artifactFingerprint,
          prepared.features,
          prepared.strategy,
          prepared.emission.auditEvents,
          prepared.stats
        )
      }

  private def prepareFeatures[
      V,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      plan: WLPlan[V, N, E, NodeKey, EdgeKey]
  ): Either[
    WLCompileError[V],
    FeaturePreparation[V, N, E, NodeKey, EdgeKey]
  ] =
    given Hash[NodeKey] = plan.nodeHash
    given Hash[EdgeKey] = plan.edgeHash

    var traces = Vector.empty[
      (
          WLRefinement[N, E, NodeKey, EdgeKey],
          WLTrace[V, NodeKey, EdgeKey]
      )
    ]
    var refinementExecutions = 0
    var subtreeExtractions = 0
    var assignmentExtractions = 0

    def traceFor(
        refinement: WLRefinement[N, E, NodeKey, EdgeKey]
    ): Either[WLCompileError[V], WLTrace[V, NodeKey, EdgeKey]] =
      traces
        .find(_._1.planNodeId.number == refinement.planNodeId.number)
        .map(_._2) match
        case Some(trace) =>
          Right(trace)
        case None =>
          WLTrace
            .prepare(plan.samples, refinement)
            .left
            .map(WLCompileError.Trace(_))
            .map { trace =>
              traces = traces :+ (refinement -> trace)
              refinementExecutions += 1
              trace
            }

    def prepare(
        expression: WLKernelExpr[N, E, NodeKey, EdgeKey]
    ): Either[WLCompileError[V], Unit] =
      expression match
        case node: WLKernelExpr.Subtree[N, E, NodeKey, EdgeKey] =>
          traceFor(node.refinement).map { _ =>
            subtreeExtractions += 1
            ()
          }

        case node: WLKernelExpr.OptimalAssignment[
              N,
              E,
              NodeKey,
              EdgeKey
            ] =>
          traceFor(node.refinement).map { _ =>
            assignmentExtractions += 1
            ()
          }

        case node: WLKernelExpr.Scale[N, E, NodeKey, EdgeKey] =>
          prepare(node.child)

        case node: WLKernelExpr.Sum[N, E, NodeKey, EdgeKey] =>
          for
            _ <- prepare(node.left)
            _ <- prepare(node.right)
          yield ()

        case node: WLKernelExpr.Normalize[N, E, NodeKey, EdgeKey] =>
          prepare(node.child)

    prepare(plan.kernel.expression).flatMap { _ =>
      WLFeatureEmitter
        .emitPrepared(
          plan.samples.map(_.key),
          plan.kernel,
          plan.options.diagonalTolerance,
          traces.map { case (refinement, trace) => refinement.planNodeId -> trace }
        )
        .left
        .map(WLCompileError.PortableFeatures(_))
        .flatMap { emission =>
          val strategy =
            WLExecutionPlanner.select(
              emission.rows,
              emission.dictionary.size,
              plan.graph,
              plan.options
            )
          portableColumns(emission).flatMap { coordinates =>
            WLFeatureMatrix
              .materializePortable(
                plan.samples.map(_.key),
                emission.rows,
                coordinates
              )
              .left
              .map(WLCompileError.FeatureMaterialization(_))
              .map { features =>
                FeaturePreparation(
                  emission,
                  traces,
                  features,
                  strategy,
                  WLExecutionStats(
                    refinementExecutions,
                    subtreeExtractions,
                    assignmentExtractions
                  )
                )
              }
          }
        }
    }

  private def portableColumns(
      emission: WLFeatureEmission
  ): Either[
    WLCompileError[Nothing],
    Vector[WLFeatureCoordinate]
  ] =
    val converted = Vector.newBuilder[WLFeatureCoordinate]
    var column = 0
    while column < emission.dictionary.size do
      emission.dictionary.origin(column) match
        case None =>
          return Left(
            WLCompileError.MissingPortableCoordinateOrigin(column)
          )
        case Some(origin) =>
          val portable = emission.dictionary.coordinates(column)
          val feature =
            portable.feature match
              case PortableWLFeature.Subtree(round, _) =>
                WLCompiledFeature.Subtree(
                  WLSubtreeFeature(round, origin)
                )
              case PortableWLFeature.OptimalAssignment(
                    _,
                    _,
                    threshold
                  ) =>
                WLCompiledFeature.OptimalAssignment(
                  HierarchyFeature(
                    WLHierarchyNode.Signature(origin),
                    threshold
                  )
                )
          converted += WLFeatureCoordinate(
            portable.blockPath,
            feature
          )
      column += 1
    Right(converted.result())

  private[engine] def sameCoordinate(
      left: WLFeatureCoordinate,
      right: WLFeatureCoordinate
  ): Boolean =
    left.blockPath == right.blockPath &&
      ((left.feature, right.feature) match
        case (
              WLCompiledFeature.Subtree(first),
              WLCompiledFeature.Subtree(second)
            ) =>
          WLSubtreeFeatures.sameFeature(first, second)
        case (
              WLCompiledFeature.OptimalAssignment(first),
              WLCompiledFeature.OptimalAssignment(second)
            ) =>
          WLOAThresholdFeatures.sameFeature(first, second)
        case _ =>
          false)

  private[engine] def coordinateHash(
      coordinate: WLFeatureCoordinate
  ): Int =
    var result = 1
    var pathIndex = 0
    while pathIndex < coordinate.blockPath.size do
      result = 31 * result + coordinate.blockPath(pathIndex)
      pathIndex += 1
    coordinate.feature match
      case WLCompiledFeature.Subtree(feature) =>
        result = 31 * result + 1
        result = 31 * result + feature.round
        31 * result + feature.signature.hashCode()
      case WLCompiledFeature.OptimalAssignment(feature) =>
        result = 31 * result + 2
        result = 31 * result + feature.node.hashCode()
        31 * result + feature.multiplicityThreshold
