package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*

enum ShortestPathAPSPAlgorithm derives CanEqual:
  case UnweightedBfs
  case WeightedDijkstra

enum ShortestPathNumericalRealization derives CanEqual:
  case ExactSparseFeatures
  case ExactPairwise
  case DualValidatedFeatures

enum CategoricalShortestPathLowering derives CanEqual:
  case ExactFeatures
  case Pairwise
  case ValidateBoth

enum ShortestPathPlanNodeKind derives CanEqual:
  case PackCorpus
  case UnweightedBfsAPSP
  case WeightedDijkstraAPSP
  case CategoricalFeatures
  case CategoricalPairwise
  case SoftPairwise
  case Normalize
  case DenotationValidation

final case class ShortestPathPlanNode(
    id: PlanNodeId,
    kind: ShortestPathPlanNodeKind,
    inputs: Vector[PlanNodeId]
)

final class ShortestPathPlanGraph private[engine] (
    val nodes: Vector[ShortestPathPlanNode],
    val root: PlanNodeId,
    val apspNodeId: PlanNodeId
):
  def node(id: PlanNodeId): Option[ShortestPathPlanNode] =
    nodes.find(_.id.number == id.number)

  def consumers(id: PlanNodeId): Vector[ShortestPathPlanNode] =
    nodes.filter(
      _.inputs.exists(_.number == id.number)
    )

  def count(kind: ShortestPathPlanNodeKind): Int =
    nodes.count(_.kind == kind)

object ShortestPathPlanGraph:
  private[engine] def categorical(
      recipeNodeId: PlanNodeId,
      lowering: CategoricalShortestPathLowering
  ): ShortestPathPlanGraph =
    val pack = PlanNodeId.fresh()
    val apsp = PlanNodeId.fresh()
    val feature = recipeNodeId
    val pairwise = PlanNodeId.fresh()
    val common =
      Vector(
        ShortestPathPlanNode(
          pack,
          ShortestPathPlanNodeKind.PackCorpus,
          Vector.empty
        ),
        ShortestPathPlanNode(
          apsp,
          ShortestPathPlanNodeKind.UnweightedBfsAPSP,
          Vector(pack)
        )
      )
    lowering match
      case CategoricalShortestPathLowering.ExactFeatures =>
        new ShortestPathPlanGraph(
          common :+
            ShortestPathPlanNode(
              feature,
              ShortestPathPlanNodeKind.CategoricalFeatures,
              Vector(apsp)
            ),
          feature,
          apsp
        )
      case CategoricalShortestPathLowering.Pairwise =>
        new ShortestPathPlanGraph(
          common :+
            ShortestPathPlanNode(
              pairwise,
              ShortestPathPlanNodeKind.CategoricalPairwise,
              Vector(apsp)
            ),
          pairwise,
          apsp
        )
      case CategoricalShortestPathLowering.ValidateBoth =>
        val validation = PlanNodeId.fresh()
        new ShortestPathPlanGraph(
          common ++
            Vector(
              ShortestPathPlanNode(
                feature,
                ShortestPathPlanNodeKind.CategoricalFeatures,
                Vector(apsp)
              ),
              ShortestPathPlanNode(
                pairwise,
                ShortestPathPlanNodeKind.CategoricalPairwise,
                Vector(apsp)
              ),
              ShortestPathPlanNode(
                validation,
                ShortestPathPlanNodeKind.DenotationValidation,
                Vector(feature, pairwise)
              )
            ),
          validation,
          apsp
        )

  private[engine] def soft[N, E, Key](
      expression: SoftShortestPathKernelExpr[N, E, Key]
  ): ShortestPathPlanGraph =
    val pack = PlanNodeId.fresh()
    val apsp = PlanNodeId.fresh()
    val common =
      Vector(
        ShortestPathPlanNode(
          pack,
          ShortestPathPlanNodeKind.PackCorpus,
          Vector.empty
        ),
        ShortestPathPlanNode(
          apsp,
          ShortestPathPlanNodeKind.WeightedDijkstraAPSP,
          Vector(pack)
        )
      )

    def outputNodes(
        current: SoftShortestPathKernelExpr[N, E, Key]
    ): Vector[ShortestPathPlanNode] =
      current match
        case node: SoftShortestPathKernelExpr.Convolution[N, E, Key] =>
          Vector(
            ShortestPathPlanNode(
              node.planNodeId,
              ShortestPathPlanNodeKind.SoftPairwise,
              Vector(apsp)
            )
          )
        case node: SoftShortestPathKernelExpr.Normalize[N, E, Key] =>
          outputNodes(node.child) :+
            ShortestPathPlanNode(
              node.planNodeId,
              ShortestPathPlanNodeKind.Normalize,
              Vector(node.child.planNodeId)
            )

    new ShortestPathPlanGraph(
      common ++ outputNodes(expression),
      expression.planNodeId,
      apsp
    )

final case class ShortestPathPreparedCapabilities(
    exactFeatures: Boolean,
    gram: Boolean,
    operator: Boolean,
    provenance: Boolean,
    cross: Boolean
)

final class ShortestPathPlanReport private[engine] (
    val graph: ShortestPathPlanGraph,
    val algorithm: ShortestPathAPSPAlgorithm,
    val realization: ShortestPathNumericalRealization,
    val capabilities: ShortestPathPreparedCapabilities,
    val semanticFingerprint: Fingerprintability,
    val sampleCount: Int,
    val normalized: Boolean,
    val featureCount: Option[Int],
    val featureNonZeros: Option[Int],
    val operatorRealization: KernelOperatorRealization,
    val apspStats: ShortestPathAPSPStats,
    val denotationMaximumDifference: Option[Double]
):
  def explain: String =
    val nodeLines =
      graph.nodes.map { node =>
        val inputs =
          if node.inputs.isEmpty then "none"
          else node.inputs.map(_.number).mkString(",")
        val consumers =
          graph.consumers(node.id).map(_.id.number)
        val renderedConsumers =
          if consumers.isEmpty then "none"
          else consumers.mkString(",")
        s"  #${node.id.number} ${node.kind} inputs=$inputs consumers=$renderedConsumers"
      }
    (
      Vector(
        "Shortest-path execution plan",
        s"root: #${graph.root.number}",
        s"APSP: $algorithm",
        s"realization: $realization",
        "nodes:"
      ) ++
        nodeLines ++
        Vector(
          "capabilities:",
          s"  exact features: ${capabilities.exactFeatures}",
          s"  Gram: ${capabilities.gram}",
          s"  operator: ${capabilities.operator} ($operatorRealization)",
          s"  provenance: ${capabilities.provenance}",
          s"  cross: ${capabilities.cross}",
          s"normalization: $normalized",
          s"feature count: ${featureCount.fold("unavailable")(_.toString)}",
          s"feature nnz: ${featureNonZeros.fold("unavailable")(_.toString)}",
          s"APSP graph transforms: ${apspStats.graphTransforms}",
          s"APSP source searches: ${apspStats.sourceSearches}",
          s"denotation max difference: ${denotationMaximumDifference.fold("not checked")(_.toString)}"
        )
    ).mkString("\n")

final case class CategoricalShortestPathPlanOptions(
    lowering: CategoricalShortestPathLowering = CategoricalShortestPathLowering.ExactFeatures,
    gramExecution: GramExecution = GramExecution.Sequential,
    validationTolerance: NonNegativeFinite = NonNegativeFinite.zero
)

final class CategoricalShortestPathPlan[V, N, E, Key] private[engine] (
    private[engine] val samples: Vector[
      GraphSample[V, N, E]
    ],
    private[engine] val recipe: UnweightedShortestPathRecipe[
      N,
      E,
      Key
    ],
    val options: CategoricalShortestPathPlanOptions,
    val graph: ShortestPathPlanGraph
)(using private[engine] val keyHash: Hash[Key]):
  def run(): Either[
    CategoricalShortestPathCompileError[V],
    PreparedCategoricalShortestPathKernel[V, Key]
  ] =
    CategoricalShortestPathCompiler.run(this)

final case class SoftShortestPathPlanOptions(
    diagonalTolerance: NonNegativeFinite = NonNegativeFinite.zero
)

final class SoftShortestPathPlan[V, N, E, Key] private[engine] (
    private[engine] val samples: Vector[
      GraphSample[V, N, E]
    ],
    private[engine] val kernel: SoftShortestPathKernel[
      N,
      E,
      Key
    ],
    val options: SoftShortestPathPlanOptions,
    val graph: ShortestPathPlanGraph
):
  def run(): Either[
    SoftShortestPathCompileError[V],
    PlannedSoftShortestPathKernel[V]
  ] =
    SoftShortestPathPlanner.run(this)

final class PlannedSoftShortestPathKernel[V] private[engine] (
    val compiled: CompiledSoftShortestPathKernel[V],
    val capabilities: ShortestPathPreparedCapabilities,
    val planReport: ShortestPathPlanReport
):
  def gram: GramMatrix =
    compiled.gram

  def operator: KernelOperator =
    compiled.operator

  def kernel(
      leftGraph: Int,
      rightGraph: Int
  ): Option[Double] =
    compiled.kernel(leftGraph, rightGraph)

object ShortestPathPlanner:
  def categorical[V, N, E, Key: Hash](
      samples: Vector[GraphSample[V, N, E]],
      recipe: UnweightedShortestPathRecipe[N, E, Key],
      options: CategoricalShortestPathPlanOptions = CategoricalShortestPathPlanOptions()
  ): CategoricalShortestPathPlan[V, N, E, Key] =
    new CategoricalShortestPathPlan(
      samples,
      recipe,
      options,
      ShortestPathPlanGraph.categorical(
        recipe.planNodeId,
        options.lowering
      )
    )

  def soft[V, N, E, Key](
      samples: Vector[GraphSample[V, N, E]],
      kernel: SoftShortestPathKernel[N, E, Key],
      options: SoftShortestPathPlanOptions = SoftShortestPathPlanOptions()
  ): SoftShortestPathPlan[V, N, E, Key] =
    new SoftShortestPathPlan(
      samples,
      kernel,
      options,
      ShortestPathPlanGraph.soft(kernel.expression)
    )

private object SoftShortestPathPlanner:
  def run[V, N, E, Key](
      plan: SoftShortestPathPlan[V, N, E, Key]
  ): Either[
    SoftShortestPathCompileError[V],
    PlannedSoftShortestPathKernel[V]
  ] =
    SoftShortestPathCompiler
      .preparePlanned(
        plan.samples,
        plan.kernel,
        plan.options.diagonalTolerance,
        plan.graph.apspNodeId
      )
      .map { compiled =>
        val capabilities =
          ShortestPathPreparedCapabilities(
            exactFeatures = false,
            gram = true,
            operator = true,
            provenance = false,
            cross = false
          )
        val report =
          new ShortestPathPlanReport(
            plan.graph,
            ShortestPathAPSPAlgorithm.WeightedDijkstra,
            ShortestPathNumericalRealization.ExactPairwise,
            capabilities,
            compiled.semanticFingerprint,
            compiled.sampleKeys.size,
            compiled.normalized,
            None,
            None,
            compiled.operator.realization,
            compiled.executionStats.apsp,
            None
          )
        new PlannedSoftShortestPathKernel(
          compiled,
          capabilities,
          report
        )
      }
