package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*

enum WLPlanNodeKind derives CanEqual:
  case Refinement
  case SubtreeFeatures
  case OptimalAssignmentThresholdFeatures
  case Scale
  case DirectSum
  case Normalize

final case class WLPlanNode(
    id: PlanNodeId,
    kind: WLPlanNodeKind,
    inputs: Vector[PlanNodeId]
)

final class WLPlanGraph private[engine] (
    val nodes: Vector[WLPlanNode],
    val root: PlanNodeId
):
  def node(id: PlanNodeId): Option[WLPlanNode] =
    nodes.find(_.id.number == id.number)

  def consumers(id: PlanNodeId): Vector[WLPlanNode] =
    nodes.filter(_.inputs.exists(_.number == id.number))

  def count(kind: WLPlanNodeKind): Int =
    nodes.count(_.kind == kind)

object WLPlanGraph:
  private[engine] def from[N, E, NodeKey, EdgeKey](
      expression: WLKernelExpr[N, E, NodeKey, EdgeKey]
  ): WLPlanGraph =
    var nodes = Vector.empty[WLPlanNode]

    def append(node: WLPlanNode): Unit =
      if !nodes.exists(_.id.number == node.id.number) then nodes = nodes :+ node

    def visit(
        current: WLKernelExpr[N, E, NodeKey, EdgeKey]
    ): Unit =
      current match
        case node: WLKernelExpr.Subtree[N, E, NodeKey, EdgeKey] =>
          append(
            WLPlanNode(
              node.refinement.planNodeId,
              WLPlanNodeKind.Refinement,
              Vector.empty
            )
          )
          append(
            WLPlanNode(
              node.planNodeId,
              WLPlanNodeKind.SubtreeFeatures,
              Vector(node.refinement.planNodeId)
            )
          )
        case node: WLKernelExpr.OptimalAssignment[
              N,
              E,
              NodeKey,
              EdgeKey
            ] =>
          append(
            WLPlanNode(
              node.refinement.planNodeId,
              WLPlanNodeKind.Refinement,
              Vector.empty
            )
          )
          append(
            WLPlanNode(
              node.planNodeId,
              WLPlanNodeKind.OptimalAssignmentThresholdFeatures,
              Vector(node.refinement.planNodeId)
            )
          )
        case node: WLKernelExpr.Scale[N, E, NodeKey, EdgeKey] =>
          visit(node.child)
          append(
            WLPlanNode(
              node.planNodeId,
              WLPlanNodeKind.Scale,
              Vector(node.child.planNodeId)
            )
          )
        case node: WLKernelExpr.Sum[N, E, NodeKey, EdgeKey] =>
          visit(node.left)
          visit(node.right)
          append(
            WLPlanNode(
              node.planNodeId,
              WLPlanNodeKind.DirectSum,
              Vector(
                node.left.planNodeId,
                node.right.planNodeId
              )
            )
          )
        case node: WLKernelExpr.Normalize[N, E, NodeKey, EdgeKey] =>
          visit(node.child)
          append(
            WLPlanNode(
              node.planNodeId,
              WLPlanNodeKind.Normalize,
              Vector(node.child.planNodeId)
            )
          )

    visit(expression)
    new WLPlanGraph(nodes, expression.planNodeId)

final case class WLPlanOptions(
    corpusContent: Option[CorpusContentFingerprint] = None,
    diagonalTolerance: NonNegativeFinite = NonNegativeFinite.zero
)

final class WLPlan[V, N, E, NodeKey, EdgeKey] private[engine] (
    private[engine] val samples: Vector[GraphSample[V, N, E]],
    private[engine] val kernel: WLKernel[N, E, NodeKey, EdgeKey],
    val options: WLPlanOptions,
    val graph: WLPlanGraph,
    val semanticFingerprint: Fingerprintability,
    val artifactFingerprint: ArtifactFingerprintability
)(using
    private[engine] val nodeHash: Hash[NodeKey],
    private[engine] val edgeHash: Hash[EdgeKey]
):
  def run(): Either[
    WLCompileError[V],
    WLCompiledKernel[V, N, E, NodeKey, EdgeKey]
  ] =
    WLCompiler.run(this)

object WLPlan:
  private[engine] def create[
      V,
      N,
      E,
      NodeKey: Hash,
      EdgeKey: Hash
  ](
      samples: Vector[GraphSample[V, N, E]],
      kernel: WLKernel[N, E, NodeKey, EdgeKey],
      options: WLPlanOptions
  ): WLPlan[V, N, E, NodeKey, EdgeKey] =
    val semantic = kernel.fingerprintability
    val artifact =
      ArtifactFingerprintability.from(
        semantic,
        options.corpusContent
      ) { (semanticFingerprint, corpusFingerprint) =>
        ArtifactFingerprintInput(
          semantic = semanticFingerprint,
          corpus = corpusFingerprint,
          orderedSamples = samples.map(_.key),
          compilerSchema = WLPlanIds.compilerSchema,
          loweringSchema = WLPlanIds.loweringSchema,
          numericalPolicy = WLPlanIds.numericalPolicy(
            options.diagonalTolerance
          ),
          approximation = None,
          engineVersion = WLPlanIds.engineVersion
        )
      }
    new WLPlan(
      samples,
      kernel,
      options,
      WLPlanGraph.from(kernel.expression),
      semantic,
      artifact
    )

private object WLPlanIds:
  val compilerSchema: StableId =
    stableId("grakern.closed-wl-compiler.v1")
  val loweringSchema: StableId =
    stableId("exact-wl-sparse-features.v1")
  def numericalPolicy(
      diagonalTolerance: NonNegativeFinite
  ): StableId =
    stableId(
      s"ordered-double-reduction.v1;diagonal-tolerance=${diagonalTolerance.toDouble}"
    )
  val engineVersion: StableId =
    stableId("grakern-engine-e4")

  private def stableId(value: String): StableId =
    StableId
      .from(value)
      .fold(
        error =>
          throw new IllegalStateException(
            s"invalid internal stable id $value: $error"
          ),
        identity
      )
