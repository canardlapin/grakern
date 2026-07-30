package grakern.engine

import grakern.core.*

final case class WLPreparedCapabilities(
    exactFeatures: Boolean,
    gram: Boolean,
    cross: Boolean,
    operator: Boolean,
    queryAlignment: Boolean,
    provenance: Boolean
)

final case class WLPlanCostEstimate(
    featureNonZeros: Long,
    gramPairs: Long,
    operatorIntermediateDimension: Int
)

final case class WLPlanMemoryEstimate(
    featurePayloadBytes: Long,
    materializedGramBytes: Long
)

final class WLPlanReport private[engine] (
    val graph: WLPlanGraph,
    val semanticFingerprint: Fingerprintability,
    val artifactFingerprint: ArtifactFingerprintability,
    val exact: Boolean,
    val sampleCount: Int,
    val featureCount: Int,
    val featureNonZeros: Int,
    val featureDensity: Double,
    val normalized: Boolean,
    val queryAlignmentRetainsNovelNorm: Boolean,
    val gramRealization: String,
    val gramExecution: GramExecution,
    val operatorRealization: KernelOperatorRealization,
    val cost: WLPlanCostEstimate,
    val memory: WLPlanMemoryEstimate,
    val executionStats: WLExecutionStats
):
  def explain: String =
    val nodeLines =
      graph.nodes.map { node =>
        val inputs =
          if node.inputs.isEmpty then "none"
          else node.inputs.map(_.number).mkString(",")
        val consumers =
          graph
            .consumers(node.id)
            .map(_.id.number)
        val renderedConsumers =
          if consumers.isEmpty then "none"
          else consumers.mkString(",")
        s"  #${node.id.number} ${node.kind} inputs=$inputs consumers=$renderedConsumers"
      }
    val identityLines =
      Vector(
        renderSemantic,
        renderArtifact
      )
    (
      Vector(
        "WL execution plan",
        s"root: #${graph.root.number}",
        "nodes:"
      ) ++
        nodeLines ++
        Vector(
          "identity:"
        ) ++
        identityLines.map(value => s"  $value") ++
        Vector(
          "realizations:",
          s"  exact features: $exact",
          s"  features: $sampleCount x $featureCount, nnz=$featureNonZeros, density=$featureDensity",
          s"  gram: $gramRealization",
          s"  gram execution: ${gramExecution.description}",
          s"  operator: $operatorRealization",
          s"  normalization: $normalized",
          s"  query alignment: frozen dictionary with novel squared norm = $queryAlignmentRetainsNovelNorm",
          "estimates:",
          s"  feature payload bytes: ${memory.featurePayloadBytes}",
          s"  materialized gram bytes: ${memory.materializedGramBytes}",
          s"  gram pairs: ${cost.gramPairs}",
          s"  operator intermediate dimension: ${cost.operatorIntermediateDimension}",
          "execution:",
          s"  refinement executions: ${executionStats.refinementExecutions}",
          s"  subtree extractions: ${executionStats.subtreeExtractions}",
          s"  OA threshold extractions: ${executionStats.optimalAssignmentExtractions}"
        )
    ).mkString("\n")

  private def renderSemantic: String =
    semanticFingerprint match
      case Fingerprintability.Durable(fingerprint) =>
        s"semantic fingerprint: durable ${fingerprint.value}"
      case Fingerprintability.Ephemeral(blockers) =>
        s"semantic fingerprint: ephemeral; blockers=${blockers.toVector.map(renderSemanticBlocker).mkString("; ")}"

  private def renderArtifact: String =
    artifactFingerprint match
      case ArtifactFingerprintability.Durable(fingerprint) =>
        s"artifact fingerprint: durable ${fingerprint.value}"
      case ArtifactFingerprintability.Ephemeral(blockers) =>
        s"artifact fingerprint: unavailable; blockers=${blockers.toVector.map(renderArtifactBlocker).mkString("; ")}"

  private def renderArtifactBlocker(
      blocker: ArtifactFingerprintBlocker
  ): String =
    blocker match
      case ArtifactFingerprintBlocker.Semantic(problem) =>
        renderSemanticBlocker(problem)
      case ArtifactFingerprintBlocker.MissingCorpusContentFingerprint =>
        "missing corpus content fingerprint"

  private def renderSemanticBlocker(
      blocker: FingerprintBlocker
  ): String =
    blocker match
      case FingerprintBlocker.AnonymousProjection(node) =>
        s"anonymous projection at node ${node.number}"
      case FingerprintBlocker.MissingCodec(node, role) =>
        s"missing $role codec at node ${node.number}"
      case FingerprintBlocker.UnsafeAssumption(node, claim) =>
        s"unsafe $claim assumption at node ${node.number}"

private[grakern] object WLPlanReport:
  def from[Coordinate, Metadata](
      graph: WLPlanGraph,
      semanticFingerprint: Fingerprintability,
      artifactFingerprint: ArtifactFingerprintability,
      features: FeatureMatrix[Coordinate, Metadata],
      operator: KernelOperator,
      gramExecution: GramExecution,
      executionStats: WLExecutionStats
  ): WLPlanReport =
    val sampleCount = features.values.rows
    val featureCount = features.values.cols
    val nonZeros = features.values.nnz
    val slots = sampleCount.toLong * featureCount.toLong
    val density =
      if slots == 0L then 0.0
      else nonZeros.toDouble / slots.toDouble
    val gramPairs =
      sampleCount.toLong * (sampleCount.toLong + 1L) / 2L
    val featurePayloadBytes =
      nonZeros.toLong * (java.lang.Double.BYTES + Integer.BYTES) +
        (sampleCount.toLong + 1L) * Integer.BYTES
    val gramBytes =
      sampleCount.toLong * sampleCount.toLong *
        java.lang.Double.BYTES
    new WLPlanReport(
      graph,
      semanticFingerprint,
      artifactFingerprint,
      exact = true,
      sampleCount,
      featureCount,
      nonZeros,
      density,
      normalized = graph.count(WLPlanNodeKind.Normalize) > 0,
      queryAlignmentRetainsNovelNorm = true,
      gramRealization = "deterministic exact feature row products",
      gramExecution,
      operator.realization,
      WLPlanCostEstimate(
        nonZeros.toLong,
        gramPairs,
        featureCount
      ),
      WLPlanMemoryEstimate(featurePayloadBytes, gramBytes),
      executionStats
    )
