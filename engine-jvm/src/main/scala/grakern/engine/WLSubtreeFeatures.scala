package grakern.engine

import grakern.core.*
import grakern.standard.wl.*
import scala.collection.mutable

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
    provenanceThunk: () => Vector[WLSubtreeProvenance[V]]
):
  lazy val provenance: Vector[WLSubtreeProvenance[V]] =
    provenanceThunk()

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
      val counts =
        new Array[Int](trace.signatureCount)
      val coloursByIndex =
        new Array[AnyRef](trace.signatureCount)
      val touched =
        mutable.ArrayBuffer.empty[Int]
      val rows =
        Vector.tabulate(trace.sampleKeys.size) { graphIndex =>
          val entries = Vector.newBuilder[(WLSubtreeFeature, Double)]
          var round = 0
          while round <= trace.rounds do
            val scale = math.sqrt(roundWeights(round).toDouble)
            if scale > 0.0 then
              touched.clear()
              val colours =
                trace.levels(round)
              val range =
                trace.graphRanges(graphIndex)
              var vertexIndex = range.start
              while vertexIndex < range.end do
                val colourIndex =
                  colours(vertexIndex).index
                if counts(colourIndex) == 0 then
                  touched += colourIndex
                  coloursByIndex(colourIndex) = colours(vertexIndex)
                counts(colourIndex) += 1
                vertexIndex += 1
              var touchedIndex = 0
              while touchedIndex < touched.size do
                val colourIndex =
                  touched(touchedIndex)
                entries +=
                  WLSubtreeFeature(
                    round,
                    coloursByIndex(colourIndex)
                      .asInstanceOf[WLColour]
                  ) ->
                    (scale * counts(colourIndex).toDouble)
                counts(colourIndex) = 0
                touchedIndex += 1
            round += 1
          ExactFeatureRow(entries.result())
        }
      Right(
        new WLSubtreeFeatures(
          trace.refinementNodeId,
          rows,
          () => buildProvenance(trace)
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
    val features =
      new Array[AnyRef](trace.signatureCount)
    val occurrences =
      new Array[AnyRef](trace.signatureCount)
    val order =
      mutable.ArrayBuffer.empty[Int]
    var round = 0
    while round <= trace.rounds do
      var vertexIndex = 0
      while vertexIndex < trace.vertexCount do
        val colour =
          trace.levels(round)(vertexIndex)
        val colourIndex =
          colour.index
        val feature =
          WLSubtreeFeature(round, colour)
        var sources =
          occurrences(colourIndex)
            .asInstanceOf[
              mutable.ArrayBuffer[WLSource[V]]
            ]
        if sources == null then
          sources = mutable.ArrayBuffer.empty[WLSource[V]]
          features(colourIndex) = feature
          occurrences(colourIndex) = sources
          order += colourIndex
        sources += trace.sources(vertexIndex)
        vertexIndex += 1
      round += 1
    order.iterator.map { colourIndex =>
      WLSubtreeProvenance(
        features(colourIndex)
          .asInstanceOf[WLSubtreeFeature],
        occurrences(colourIndex)
          .asInstanceOf[
            mutable.ArrayBuffer[WLSource[V]]
          ]
          .toVector
      )
    }.toVector
