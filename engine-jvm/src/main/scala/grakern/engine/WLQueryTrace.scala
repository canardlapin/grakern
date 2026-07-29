package grakern.engine

import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import scala.collection.mutable.ArrayBuffer

private[grakern] enum WLQueryTraceError[V]:
  case RefinementMismatch(
      training: PlanNodeId,
      query: PlanNodeId
  )
  case Packing(error: PackedCorpusError[V])
  case Interning(
      level: Int,
      source: WLSource[V],
      error: WLInternerError
  )

final class WLQueryTrace[V, NodeKey, EdgeKey] private[engine] (
    val refinementNodeId: PlanNodeId,
    val sampleKeys: Vector[SampleKey],
    private[engine] val graphRanges: Vector[Range],
    private[engine] val sources: Vector[WLSource[V]],
    private[engine] val levels: Vector[Vector[WLColour]],
    private[grakern] val overlay: WLInternerOverlay[NodeKey, EdgeKey]
):
  def rounds: Int = levels.size - 1
  def vertexCount: Int = sources.size
  def trainingSignatureCount: Int = overlay.baseSignatureCount
  def novelSignatureCount: Int = overlay.novelSignatureCount

  def source(vertexIndex: Int): Option[WLSource[V]] =
    sources.lift(vertexIndex)

  def colour(level: Int, vertexIndex: Int): Option[WLColour] =
    levels.lift(level).flatMap(_.lift(vertexIndex))

  def signature(
      colour: WLColour
  ): Option[WLSignature[NodeKey, EdgeKey]] =
    overlay.signature(colour)

  def isNovel(colour: WLColour): Boolean =
    overlay.isNovel(colour)

  def semanticallyEquivalent[OtherV](
      left: WLColour,
      that: WLQueryTrace[OtherV, NodeKey, EdgeKey],
      right: WLColour
  ): Boolean =
    overlay.semanticallyEquivalent(left, that.overlay, right)

  def graphColours(
      graphIndex: Int,
      level: Int
  ): Option[Vector[WLColour]] =
    for
      range <- graphRanges.lift(graphIndex)
      colours <- levels.lift(level)
    yield range.iterator.map(colours).toVector

private[grakern] object WLQueryTrace:
  def prepare[
      TrainingV,
      QueryV,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      training: WLTrace[TrainingV, NodeKey, EdgeKey],
      samples: Vector[GraphSample[QueryV, N, E]],
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  ): Either[
    WLQueryTraceError[QueryV],
    WLQueryTrace[QueryV, NodeKey, EdgeKey]
  ] =
    if training.refinementNodeId.number !=
        refinement.planNodeId.number
    then
      Left(
        WLQueryTraceError.RefinementMismatch(
          training.refinementNodeId,
          refinement.planNodeId
        )
      )
    else
      PackedGraphCorpus
        .pack(samples)
        .left
        .map(WLQueryTraceError.Packing(_))
        .flatMap(packed => build(training, packed, refinement))

  private def build[
      TrainingV,
      QueryV,
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      training: WLTrace[TrainingV, NodeKey, EdgeKey],
      packed: PackedGraphCorpus[QueryV, N, E],
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  ): Either[
    WLQueryTraceError[QueryV],
    WLQueryTrace[QueryV, NodeKey, EdgeKey]
  ] =
    val nodeKeys =
      packed.categoricalNodeColumn(value => refinement.nodeKey(value))
    val incident =
      Array.fill(packed.vertexCount)(
        ArrayBuffer.empty[(Int, EdgeKey)]
      )
    packed.packedEdges.foreach { edge =>
      val key = refinement.edgeKey(edge.value)
      incident(edge.source) += ((edge.target, key))
      incident(edge.target) += ((edge.source, key))
    }
    val sources =
      Vector.tabulate(packed.vertexCount) { vertexIndex =>
        WLSource(
          packed.ownerOfVertex(vertexIndex),
          packed.originalVertex(vertexIndex)
        )
      }
    var overlay = WLInternerOverlay(training.interner)
    val initial = Vector.newBuilder[WLColour]
    nodeKeys.foreach { key =>
      val (next, colour) = overlay.internInitial(key)
      overlay = next
      initial += colour
    }
    var allLevels = Vector(initial.result())
    var previous = allLevels.head
    var round = 1
    var failure: Option[WLQueryTraceError[QueryV]] = None

    while round <= refinement.rounds && failure.isEmpty do
      val current = Vector.newBuilder[WLColour]
      var vertexIndex = 0
      while vertexIndex < packed.vertexCount && failure.isEmpty do
        val neighbourhood =
          incident(vertexIndex).iterator.map { case (target, edgeKey) =>
            edgeKey -> previous(target)
          }.toVector
        overlay.internRefined(
          previous(vertexIndex),
          neighbourhood
        ) match
          case Right((next, colour)) =>
            overlay = next
            current += colour
          case Left(error) =>
            failure = Some(
              WLQueryTraceError.Interning(
                round,
                sources(vertexIndex),
                error
              )
            )
        vertexIndex += 1
      val completed = current.result()
      if failure.isEmpty then
        allLevels = allLevels :+ completed
        previous = completed
      round += 1

    failure.toLeft(
      new WLQueryTrace(
        refinement.planNodeId,
        packed.sampleKeys,
        Vector.tabulate(packed.graphCount)(
          packed.graphVertexRange
        ),
        sources,
        allLevels,
        overlay
      )
    )
