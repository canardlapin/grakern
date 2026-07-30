package grakern.engine

import cats.kernel.Hash
import gale.linalg.LinAlgError
import grakern.core.PlanNodeId
import grakern.standard.shortestpath.*

final case class ShortestPathFeatureMetadata[Key](
    feature: ShortestPathPart[Key, HopDistance]
)

final case class ShortestPathFeatureProvenance[V, Key](
    feature: ShortestPathPart[Key, HopDistance],
    witnesses: Vector[ShortestPathWitness[V]]
)

final case class ShortestPathFeatureContribution[V, Key](
    provenance: ShortestPathFeatureProvenance[V, Key],
    value: Double
)

final case class CategoricalShortestPathFeatureStats(
    graphCount: Int,
    featureCount: Int,
    nonZeros: Int,
    emittedPatterns: Int
)

final private[grakern] class CategoricalShortestPathFeatureSet[V, Key] private[engine] (
    val apspPlanNodeId: PlanNodeId,
    val recipePlanNodeId: PlanNodeId,
    private[grakern] val rows: Vector[
      ExactFeatureRow[ShortestPathPart[Key, HopDistance]]
    ],
    val matrix: FeatureMatrix[
      ShortestPathPart[Key, HopDistance],
      ShortestPathFeatureMetadata[Key]
    ],
    val provenance: Vector[ShortestPathFeatureProvenance[V, Key]],
    val stats: CategoricalShortestPathFeatureStats
)(using keyHash: Hash[Key]):
  def kernel(
      leftGraph: Int,
      rightGraph: Int
  ): Option[Double] =
    for
      left <- rows.lift(leftGraph)
      right <- rows.lift(rightGraph)
    yield left.dot(right)(CategoricalShortestPathFeatures.sameFeature)

  def explain(
      leftGraph: Int,
      rightGraph: Int
  ): Option[Vector[ShortestPathFeatureContribution[V, Key]]] =
    for
      left <- rows.lift(leftGraph)
      right <- rows.lift(rightGraph)
    yield left.entries.flatMap { case (feature, leftValue) =>
      right.entries
        .collectFirst {
          case (candidate, rightValue)
              if CategoricalShortestPathFeatures.sameFeature(
                feature,
                candidate
              ) =>
            rightValue
        }
        .flatMap { rightValue =>
          val contribution = leftValue * rightValue
          if contribution == 0.0 then None
          else
            provenance
              .find(value =>
                CategoricalShortestPathFeatures.sameFeature(
                  value.feature,
                  feature
                )
              )
              .map(
                ShortestPathFeatureContribution(
                  _,
                  contribution
                )
              )
        }
    }

object CategoricalShortestPathFeatures:
  private[grakern] def fromTrace[V, N, E, Key: Hash](
      trace: UnweightedAPSPTrace[V, N, E],
      recipe: UnweightedShortestPathRecipe[N, E, Key]
  ): Either[
    LinAlgError,
    CategoricalShortestPathFeatureSet[V, Key]
  ] =
    val occurrences = trace.occurrences(recipe)
    var allProvenance =
      Vector.empty[ShortestPathFeatureProvenance[V, Key]]
    var emittedPatterns = 0
    val rows =
      occurrences.map { graphOccurrences =>
        val entries =
          graphOccurrences.foldLeft(
            Vector.empty[
              (
                  ShortestPathPart[Key, HopDistance],
                  Double
              )
            ]
          ) { (current, occurrence) =>
            emittedPatterns += 1
            val feature = occurrence.part
            val index =
              current.indexWhere(entry => sameFeature(entry._1, feature))
            val updated =
              if index < 0 then current :+ (feature -> 1.0)
              else
                current.updated(
                  index,
                  feature -> (current(index)._2 + 1.0)
                )
            val provenanceIndex =
              allProvenance.indexWhere(value => sameFeature(value.feature, feature))
            if provenanceIndex < 0 then
              allProvenance = allProvenance :+
                ShortestPathFeatureProvenance(
                  feature,
                  Vector(occurrence.witness)
                )
            else
              val previous = allProvenance(provenanceIndex)
              allProvenance = allProvenance.updated(
                provenanceIndex,
                previous.copy(
                  witnesses = previous.witnesses :+
                    occurrence.witness
                )
              )
            updated
          }
        ExactFeatureRow(entries)
      }
    ExactFeatureMatrix
      .materialize(
        trace.sampleKeys,
        rows,
        sameFeature,
        ShortestPathFeatureMetadata.apply
      )
      .map { matrix =>
        new CategoricalShortestPathFeatureSet(
          trace.planNodeId,
          recipe.planNodeId,
          rows,
          matrix,
          allProvenance,
          CategoricalShortestPathFeatureStats(
            graphCount = trace.graphCount,
            featureCount = matrix.columns.size,
            nonZeros = matrix.values.nnz,
            emittedPatterns
          )
        )
      }

  private[engine] def sameFeature[Key: Hash](
      left: ShortestPathPart[Key, HopDistance],
      right: ShortestPathPart[Key, HopDistance]
  ): Boolean =
    Hash[ShortestPathPart[Key, HopDistance]].eqv(left, right)
