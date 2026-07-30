package grakern.engine

import cats.kernel.Hash
import gale.linalg.LinAlgError
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.histogram.*

final case class HistogramFeatureMetadata[+Key](
    feature: HistogramFeature[Key]
)

final case class HistogramFeatureProvenance[V, +Key](
    feature: HistogramFeature[Key],
    witnesses: Vector[HistogramWitness[V]]
)

final case class HistogramFeatureContribution[V, +Key](
    provenance: HistogramFeatureProvenance[V, Key],
    value: Double
)

final case class HistogramPlanReport(
    exact: Boolean,
    sampleCount: Int,
    featureCount: Int,
    featureNonZeros: Int,
    emittedPatterns: Int,
    gramExecution: GramExecution
):
  def explain: String =
    Vector(
      "Histogram execution plan",
      "decomposition: fused nonnegative pattern fold",
      s"exact features: $exact",
      s"features: $sampleCount x $featureCount, nnz=$featureNonZeros",
      s"emitted patterns: $emittedPatterns",
      s"gram: ${gramExecution.description}",
      "operator: FeatureBacked"
    ).mkString("\n")

sealed trait HistogramCompileError

object HistogramCompileError:
  final case class Decomposition(error: DecompositionError) extends HistogramCompileError
  final case class FeatureMaterialization(error: LinAlgError) extends HistogramCompileError
  final case class Gram(error: GramExecutionFailure) extends HistogramCompileError

final class CompiledHistogramKernel[V, Key] private[engine] (
    val sampleKeys: Vector[SampleKey],
    private val rows: Vector[
      ExactFeatureRow[HistogramFeature[Key]]
    ],
    val features: FeatureMatrix[
      HistogramFeature[Key],
      HistogramFeatureMetadata[Key]
    ],
    val gram: GramMatrix,
    val operator: KernelOperator,
    val provenance: Vector[
      HistogramFeatureProvenance[V, Key]
    ],
    val semanticFingerprint: Fingerprintability,
    val planReport: HistogramPlanReport
)(using keyHash: Hash[Key]):
  def kernel(
      leftSample: Int,
      rightSample: Int
  ): Option[Double] =
    for
      left <- rows.lift(leftSample)
      right <- rows.lift(rightSample)
    yield left.dot(right)(sameFeature)

  def explain(
      leftSample: Int,
      rightSample: Int
  ): Option[Vector[HistogramFeatureContribution[V, Key]]] =
    for
      left <- rows.lift(leftSample)
      right <- rows.lift(rightSample)
    yield left.entries.flatMap { case (feature, leftValue) =>
      right.entries
        .collectFirst {
          case (candidate, rightValue) if sameFeature(feature, candidate) =>
            rightValue
        }
        .flatMap { rightValue =>
          provenance
            .find(value => sameFeature(value.feature, feature))
            .map(
              HistogramFeatureContribution(
                _,
                leftValue * rightValue
              )
            )
        }
    }

  private def sameFeature(
      left: HistogramFeature[Key],
      right: HistogramFeature[Key]
  ): Boolean =
    left.kind == right.kind &&
      keyHash.eqv(left.key, right.key)

object HistogramCompiler:
  def prepare[V, N, E, Key: Hash](
      samples: Vector[GraphSample[V, N, E]],
      recipe: HistogramRecipe[N, E, Key],
      gramExecution: GramExecution = GramExecution.Sequential
  ): Either[
    HistogramCompileError,
    CompiledHistogramKernel[V, Key]
  ] =
    val keyHash = Hash[Key]
    def sameFeature(
        left: HistogramFeature[Key],
        right: HistogramFeature[Key]
    ): Boolean =
      left.kind == right.kind &&
        keyHash.eqv(left.key, right.key)

    var allProvenance =
      Vector.empty[HistogramFeatureProvenance[V, Key]]
    var emittedPatterns = 0
    val rows =
      samples.map { sample =>
        recipe
          .decomposition[V]
          .fold(
            sample,
            Vector.empty[
              (
                  HistogramFeature[Key],
                  Double
              )
            ]
          ) { (current, occurrence, weight) =>
            emittedPatterns += 1
            val feature = occurrence.feature
            val index =
              current.indexWhere(entry => sameFeature(entry._1, feature))
            val updated =
              if index < 0 then current :+ (feature -> weight.toDouble)
              else
                current.updated(
                  index,
                  feature ->
                    (current(index)._2 + weight.toDouble)
                )
            val provenanceIndex =
              allProvenance.indexWhere(value => sameFeature(value.feature, feature))
            if provenanceIndex < 0 then
              allProvenance = allProvenance :+
                HistogramFeatureProvenance(
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
          .map(ExactFeatureRow(_))
      }
    val firstFailure =
      rows.collectFirst { case Left(error) => error }
    firstFailure match
      case Some(error) =>
        Left(HistogramCompileError.Decomposition(error))
      case None =>
        val completed =
          rows.collect { case Right(row) => row }
        ExactFeatureMatrix
          .materialize(
            samples.map(_.key),
            completed,
            sameFeature,
            HistogramFeatureMetadata.apply
          )
          .left
          .map(HistogramCompileError.FeatureMaterialization(_))
          .flatMap { features =>
            ExactFeatureNumericalViews
              .gram(
                features,
                completed,
                sameFeature,
                gramExecution
              )
              .left
              .map(HistogramCompileError.Gram(_))
              .map { gram =>
                val operator =
                  ExactFeatureNumericalViews.operator(features)
                new CompiledHistogramKernel(
                  samples.map(_.key),
                  completed,
                  features,
                  gram,
                  operator,
                  allProvenance,
                  recipe.fingerprintability,
                  HistogramPlanReport(
                    exact = true,
                    samples.size,
                    features.columns.size,
                    features.values.nnz,
                    emittedPatterns,
                    gramExecution
                  )
                )
              }
          }
