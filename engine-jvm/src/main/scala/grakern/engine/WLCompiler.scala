package grakern.engine

import cats.kernel.Hash
import gale.linalg.LinAlgError
import grakern.core.*
import grakern.graph4s.GraphSample
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

  final case class FeatureMaterialization(error: LinAlgError) extends WLCompileError[Nothing]

  final case class Gram(error: GramExecutionFailure) extends WLCompileError[Nothing]

final class WLCompiledKernel[V, N, E, NodeKey, EdgeKey] private[engine] (
    val sampleKeys: Vector[SampleKey],
    private[grakern] val rows: Vector[
      ExactFeatureRow[WLFeatureCoordinate]
    ],
    private[engine] val traces: Vector[
      (
          WLRefinement[N, E, NodeKey, EdgeKey],
          WLTrace[V, NodeKey, EdgeKey]
      )
    ],
    private[engine] val expression: WLKernelExpr[
      N,
      E,
      NodeKey,
      EdgeKey
    ],
    private[engine] val diagonalTolerance: NonNegativeFinite,
    val planGraph: WLPlanGraph,
    val semanticFingerprint: Fingerprintability,
    val artifactFingerprint: ArtifactFingerprintability,
    val features: FeatureMatrix[
      WLFeatureCoordinate,
      WLFeatureMetadata
    ],
    val gram: GramMatrix,
    val operator: KernelOperator,
    val capabilities: WLPreparedCapabilities,
    val planReport: WLPlanReport,
    val auditEvents: Vector[NumericalAuditEvent],
    val executionStats: WLExecutionStats
):
  def graphCount: Int = sampleKeys.size

  def activeCoordinates(graphIndex: Int): Option[Int] =
    rows.lift(graphIndex).map(_.activeSize)

  def squaredNorm(graphIndex: Int): Option[Double] =
    rows.lift(graphIndex).map(_.squaredNorm)

  def kernel(leftGraph: Int, rightGraph: Int): Option[Double] =
    for
      left <- rows.lift(leftGraph)
      right <- rows.lift(rightGraph)
    yield left.dot(right)(
      WLCompiler.coordinateHash,
      WLCompiler.sameCoordinate
    )

  def cross[QueryV](
      samples: Vector[GraphSample[QueryV, N, E]]
  ): Either[WLQueryCompileError[QueryV], WLQueryResult] =
    WLQueryAlignment.prepare(this, samples)

  private[engine] def trainingTrace(
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
      samples: Vector[GraphSample[V, N, E]],
      kernel: WLKernel[N, E, NodeKey, EdgeKey],
      options: WLPlanOptions = WLPlanOptions()
  ): WLPlan[V, N, E, NodeKey, EdgeKey] =
    WLPlan.create(samples, kernel, options)

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
    var auditEvents = Vector.empty[NumericalAuditEvent]

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

    def compile(
        expression: WLKernelExpr[N, E, NodeKey, EdgeKey]
    ): Either[
      WLCompileError[V],
      Vector[ExactFeatureRow[WLFeatureCoordinate]]
    ] =
      expression match
        case node: WLKernelExpr.Subtree[N, E, NodeKey, EdgeKey] =>
          for
            trace <- traceFor(node.refinement)
            features <- WLSubtreeFeatures
              .fromTrace(trace, node.roundWeights)
              .left
              .map(WLCompileError.Subtree(_))
          yield
            subtreeExtractions += 1
            features.rows.map(
              _.mapKeys(feature =>
                WLFeatureCoordinate(
                  Vector.empty,
                  WLCompiledFeature.Subtree(feature)
                )
              )
            )

        case node: WLKernelExpr.OptimalAssignment[
              N,
              E,
              NodeKey,
              EdgeKey
            ] =>
          for
            trace <- traceFor(node.refinement)
            observed <- WLObservedHierarchy
              .fromTrace(trace)
              .left
              .map(WLCompileError.Hierarchy(_))
          yield
            assignmentExtractions += 1
            WLOAThresholdFeatures
              .fromObserved(observed)
              .rows
              .map(
                _.mapKeys(feature =>
                  WLFeatureCoordinate(
                    Vector.empty,
                    WLCompiledFeature.OptimalAssignment(feature)
                  )
                )
              )

        case node: WLKernelExpr.Scale[N, E, NodeKey, EdgeKey] =>
          compile(node.child).map { rows =>
            val factor = math.sqrt(node.weight.toDouble)
            if factor == 0.0 then
              Vector.fill(rows.size)(
                ExactFeatureRow(Vector.empty)
              )
            else rows.map(_.scaled(factor))
          }

        case node: WLKernelExpr.Sum[N, E, NodeKey, EdgeKey] =>
          for
            left <- compile(node.left)
            right <- compile(node.right)
          yield left.zip(right).map { case (leftRow, rightRow) =>
            leftRow.mapKeys(coordinate =>
              coordinate.copy(
                blockPath = 0 +: coordinate.blockPath
              )
            ) ++
              rightRow.mapKeys(coordinate =>
                coordinate.copy(
                  blockPath = 1 +: coordinate.blockPath
                )
              )
          }

        case node: WLKernelExpr.Normalize[N, E, NodeKey, EdgeKey] =>
          compile(node.child).flatMap { rows =>
            ExactFeatureRows
              .normalize(
                plan.samples.map(_.key),
                rows,
                plan.options.diagonalTolerance
              )
              .left
              .map(WLCompileError.Normalization(_))
              .map { case (normalized, events) =>
                auditEvents = auditEvents ++ events
                normalized
              }
          }

    compile(plan.kernel.expression).flatMap { rows =>
      WLFeatureMatrix
        .materialize(plan.samples.map(_.key), rows)
        .left
        .map(WLCompileError.FeatureMaterialization(_))
        .flatMap { features =>
          ExactFeatureNumericalViews
            .gram(
              features,
              rows,
              plan.options.gramExecution
            )
            .left
            .map(WLCompileError.Gram(_))
            .map { gram =>
              val operator =
                ExactFeatureNumericalViews.operator(features)
              val capabilities =
                WLPreparedCapabilities(
                  exactFeatures = true,
                  gram = true,
                  cross = true,
                  operator = true,
                  queryAlignment = true,
                  provenance = true
                )
              val stats =
                WLExecutionStats(
                  refinementExecutions,
                  subtreeExtractions,
                  assignmentExtractions
                )
              val report =
                WLPlanReport.from(
                  plan.graph,
                  plan.semanticFingerprint,
                  plan.artifactFingerprint,
                  features,
                  operator,
                  plan.options.gramExecution,
                  stats
                )
              new WLCompiledKernel(
                plan.samples.map(_.key),
                rows,
                traces,
                plan.kernel.expression,
                plan.options.diagonalTolerance,
                plan.graph,
                plan.semanticFingerprint,
                plan.artifactFingerprint,
                features,
                gram,
                operator,
                capabilities,
                report,
                auditEvents,
                stats
              )
            }
        }
    }

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
