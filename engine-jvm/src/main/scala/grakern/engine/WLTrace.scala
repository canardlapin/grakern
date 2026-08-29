package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.standard.wl.*

enum WLTraceError[V]:
  case Packing(error: PackedCorpusError[V])
  case Interning(
      level: Int,
      source: WLSource[V],
      error: WLInternerError
  )

final class WLTrace[V, NodeKey, EdgeKey] private[engine] (
    val refinementNodeId: PlanNodeId,
    val sampleKeys: Vector[SampleKey],
    private[engine] val graphRanges: Vector[Range],
    private[engine] val sources: Vector[WLSource[V]],
    private[engine] val levels: Vector[Vector[WLColour]],
    private[grakern] val interner: WLInterner[NodeKey, EdgeKey]
) extends WLFeatureTrace[NodeKey, EdgeKey]:
  def rounds: Int = levels.size - 1
  def vertexCount: Int = sources.size
  def signatureCount: Int = interner.size
  def allColours: Vector[WLColour] = interner.colours

  def source(vertexIndex: Int): Option[WLSource[V]] =
    sources.lift(vertexIndex)

  def colour(level: Int, vertexIndex: Int): Option[WLColour] =
    levels.lift(level).flatMap(_.lift(vertexIndex))

  def signature(
      colour: WLColour
  ): Option[WLSignature[NodeKey, EdgeKey]] =
    interner.signature(colour)

  def semanticallyEquivalent[OtherV](
      left: WLColour,
      that: WLTrace[OtherV, NodeKey, EdgeKey],
      right: WLColour
  ): Boolean =
    interner.semanticallyEquivalent(left, that.interner, right)

  def occurrence(
      level: Int,
      vertexIndex: Int
  ): Option[WLSignatureOccurrence[V]] =
    for
      foundSource <- source(vertexIndex)
      foundColour <- colour(level, vertexIndex)
    yield WLSignatureOccurrence(foundSource, level, foundColour)

  def graphColours(
      graphIndex: Int,
      level: Int
  ): Option[Vector[WLColour]] =
    for
      range <- graphRanges.lift(graphIndex)
      colours <- levels.lift(level)
    yield range.iterator.map(colours).toVector

  override def foreachGraphColour(graphIndex: Int, level: Int)(
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

private[grakern] object WLTrace:
  final private[engine] class PackedIncidenceView[V, N, E, EdgeKey](
      packed: PackedGraphCorpus[V, N, E],
      edgeKeys: Vector[EdgeKey]
  ) extends WLIncidenceView[EdgeKey]:
    private var start = 0
    private var end = 0
    private var colours = Vector.empty[WLColour]

    def select(vertexIndex: Int, previous: Vector[WLColour]): Unit =
      start = packed.adjacencyStart(vertexIndex)
      end = packed.adjacencyEnd(vertexIndex)
      colours = previous

    def size: Int = end - start

    def edgeKey(index: Int): EdgeKey =
      edgeKeys(packed.adjacencyEdge(start + index))

    def orientation(index: Int): Orientation =
      packed.adjacencyOrientation(start + index)

    def colour(index: Int): WLColour =
      colours(packed.adjacencyTarget(start + index))

  def prepare[
      V,
      N,
      E,
      NodeKey: Hash,
      EdgeKey: Hash
  ](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  ): Either[WLTraceError[V], WLTrace[V, NodeKey, EdgeKey]] =
    PackedGraphCorpus
      .pack(samples)
      .left
      .map(WLTraceError.Packing(_))
      .flatMap(packed => build(packed, refinement))

  private def build[
      V,
      N,
      E,
      NodeKey: Hash,
      EdgeKey: Hash
  ](
      packed: PackedGraphCorpus[V, N, E],
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  ): Either[WLTraceError[V], WLTrace[V, NodeKey, EdgeKey]] =
    val nodeKeys =
      packed.categoricalNodeColumn(value => refinement.nodeKey(value))
    val edgeKeys =
      packed.packedEdges.map(edge => refinement.edgeKey(edge.value))
    val neighbourhood =
      new PackedIncidenceView(packed, edgeKeys)
    val sources =
      Vector.tabulate(packed.vertexCount) { vertexIndex =>
        WLSource(
          packed.ownerOfVertex(vertexIndex),
          packed.originalVertex(vertexIndex)
        )
      }
    val interner =
      WLInternerBuilder.empty(
        refinement.codecs,
        refinement.hashing,
        expectedSignatures = math
          .min(
            Int.MaxValue.toLong,
            packed.vertexCount.toLong * (refinement.rounds.toLong + 1L)
          )
          .toInt
      )
    val initial = Vector.newBuilder[WLColour]
    nodeKeys.foreach { key =>
      initial += interner.internInitial(key)
    }
    var allLevels = Vector(initial.result())
    var previous = allLevels.head
    var round = 1

    while round <= refinement.rounds do
      val current = Vector.newBuilder[WLColour]
      var vertexIndex = 0
      while vertexIndex < packed.vertexCount do
        neighbourhood.select(vertexIndex, previous)
        val colour = interner.internRefinedViewTrusted(
          previous(vertexIndex),
          neighbourhood
        )
        current += colour
        vertexIndex += 1
      val completed = current.result()
      allLevels = allLevels :+ completed
      previous = completed
      round += 1

    Right(
      new WLTrace(
        refinement.planNodeId,
        packed.sampleKeys,
        Vector.tabulate(packed.graphCount)(
          packed.graphVertexRange
        ),
        sources,
        allLevels,
        interner.freeze()
      )
    )
