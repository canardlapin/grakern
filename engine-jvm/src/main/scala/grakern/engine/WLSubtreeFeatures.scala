package grakern.engine

import grakern.core.*
import grakern.standard.wl.*

final case class WLSubtreeFeature(
    round: Int,
    signature: WLColour
)

final case class WLSubtreeProvenance[+V](
    feature: WLSubtreeFeature,
    occurrences: Vector[WLSource[V]]
)

enum WLSubtreeFeatureError:
  case WeightCount(expected: Int, observed: Int)

final class WLSubtreeFeatures[V] private[engine] (
    val refinementNodeId: PlanNodeId,
    private[grakern] val rows: Vector[
      ExactFeatureRow[WLSubtreeFeature]
    ],
    val provenance: Vector[WLSubtreeProvenance[V]]
):
  def graphCount: Int = rows.size

  def activeCoordinates(graphIndex: Int): Option[Int] =
    rows.lift(graphIndex).map(_.activeSize)

  def squaredNorm(graphIndex: Int): Option[Double] =
    rows.lift(graphIndex).map(_.squaredNorm)

  def kernel(leftGraph: Int, rightGraph: Int): Option[Double] =
    for
      left <- rows.lift(leftGraph)
      right <- rows.lift(rightGraph)
    yield left.dot(right)(WLSubtreeFeatures.sameFeature)

private[grakern] object WLSubtreeFeatures:
  def fromTrace[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey],
      roundWeights: Vector[NonNegativeFinite]
  ): Either[WLSubtreeFeatureError, WLSubtreeFeatures[V]] =
    val expected = trace.rounds + 1
    if roundWeights.size != expected then
      Left(
        WLSubtreeFeatureError.WeightCount(
          expected,
          roundWeights.size
        )
      )
    else
      val rows =
        Vector.tabulate(trace.sampleKeys.size) { graphIndex =>
          val entries = Vector.newBuilder[(WLSubtreeFeature, Double)]
          (0 to trace.rounds).foreach { round =>
            val scale = math.sqrt(roundWeights(round).toDouble)
            if scale > 0.0 then
              val colours =
                trace
                  .graphColours(graphIndex, round)
                  .getOrElse(Vector.empty)
              val counts =
                colours.foldLeft(Vector.empty[(WLColour, Int)]) { (current, colour) =>
                  val index =
                    current.indexWhere { case (candidate, _) =>
                      candidate.equals(colour)
                    }
                  if index < 0 then current :+ (colour -> 1)
                  else
                    current.updated(
                      index,
                      colour -> (current(index)._2 + 1)
                    )
                }
              counts.foreach { case (colour, count) =>
                entries +=
                  WLSubtreeFeature(round, colour) ->
                    (scale * count.toDouble)
              }
          }
          ExactFeatureRow(entries.result())
        }
      val provenance =
        buildProvenance(trace)
      Right(
        new WLSubtreeFeatures(
          trace.refinementNodeId,
          rows,
          provenance
        )
      )

  def unitWeighted[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey]
  ): WLSubtreeFeatures[V] =
    fromTrace(
      trace,
      Vector.fill(trace.rounds + 1)(NonNegativeFinite.one)
    ).toOption.get

  private[engine] def sameFeature(
      left: WLSubtreeFeature,
      right: WLSubtreeFeature
  ): Boolean =
    left.round == right.round &&
      left.signature.equals(right.signature)

  private def buildProvenance[V, NodeKey, EdgeKey](
      trace: WLTrace[V, NodeKey, EdgeKey]
  ): Vector[WLSubtreeProvenance[V]] =
    var result = Vector.empty[WLSubtreeProvenance[V]]
    (0 to trace.rounds).foreach { round =>
      (0 until trace.vertexCount).foreach { vertexIndex =>
        val occurrence =
          trace.occurrence(round, vertexIndex).get
        val feature =
          WLSubtreeFeature(round, occurrence.colour)
        val index =
          result.indexWhere(value => sameFeature(value.feature, feature))
        if index < 0 then
          result = result :+
            WLSubtreeProvenance(
              feature,
              Vector(occurrence.source)
            )
        else
          val current = result(index)
          result = result.updated(
            index,
            current.copy(
              occurrences = current.occurrences :+ occurrence.source
            )
          )
      }
    }
    result
