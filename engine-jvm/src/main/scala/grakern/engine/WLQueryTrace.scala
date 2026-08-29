package grakern.engine

import grakern.core.*
import grakern.standard.wl.*

enum WLQueryTraceError[V]:
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

  private[engine] def foreachGraphColour(graphIndex: Int, level: Int)(
      f: WLColour => Unit
  ): Boolean =
    (graphRanges.lift(graphIndex), levels.lift(level)) match
      case (Some(range), Some(colours)) =>
        var vertexIndex = range.start
        while vertexIndex < range.end do
          f(colours(vertexIndex))
          vertexIndex += 1
        true
      case _ => false

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
      samples: Vector[LabelledNeighbourhood[QueryV, N, E]],
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
    val edgeKeys =
      packed.packedEdges.map(edge => refinement.edgeKey(edge.value))
    val neighbourhood =
      new WLTrace.PackedIncidenceView(packed, edgeKeys)
    val sources =
      Vector.tabulate(packed.vertexCount) { vertexIndex =>
        WLSource(
          packed.ownerOfVertex(vertexIndex),
          packed.originalVertex(vertexIndex)
        )
      }
    val expectedSignatures = math
      .min(
        Int.MaxValue.toLong,
        packed.vertexCount.toLong * (refinement.rounds.toLong + 1L)
      )
      .toInt
    val overlay = WLInternerOverlayBuilder(training.interner, expectedSignatures)
    val initial = Vector.newBuilder[WLColour]
    nodeKeys.foreach { key =>
      initial += overlay.internInitial(key)
    }
    var allLevels = Vector(initial.result())
    var previous = allLevels.head
    var round = 1
    var failure: Option[WLQueryTraceError[QueryV]] = None

    while round <= refinement.rounds && failure.isEmpty do
      val current = Vector.newBuilder[WLColour]
      var vertexIndex = 0
      while vertexIndex < packed.vertexCount && failure.isEmpty do
        neighbourhood.select(vertexIndex, previous)
        overlay.internRefinedView(
          previous(vertexIndex),
          neighbourhood
        ) match
          case Right(colour) => current += colour
          case Left(error)   =>
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
        overlay.freeze()
      )
    )
