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
):
  def rounds: Int = levels.size - 1
  def vertexCount: Int = sources.size
  def signatureCount: Int = interner.size

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

private[grakern] object WLTrace:
  /** The refinement neighbourhood of one packed vertex: its adjacency entries in incidence order,
    * each carrying the projected edge key and the orientation relative to that vertex.
    */
  private[engine] def incidentKeys[V, N, E, NodeKey, EdgeKey](
      packed: PackedGraphCorpus[V, N, E],
      refinement: WLRefinement[N, E, NodeKey, EdgeKey]
  ): Array[Vector[(Int, EdgeKey, Orientation)]] =
    val edgeKeys = packed.packedEdges.map(edge => refinement.edgeKey(edge.value))
    Array.tabulate(packed.vertexCount) { vertexIndex =>
      val builder = Vector.newBuilder[(Int, EdgeKey, Orientation)]
      var adjacencyIndex = packed.adjacencyStart(vertexIndex)
      val end = packed.adjacencyEnd(vertexIndex)
      while adjacencyIndex < end do
        builder += (
          (
            packed.adjacencyTarget(adjacencyIndex),
            edgeKeys(packed.adjacencyEdge(adjacencyIndex)),
            packed.adjacencyOrientation(adjacencyIndex)
          )
        )
        adjacencyIndex += 1
      builder.result()
    }

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
    val incident = incidentKeys(packed, refinement)
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
        refinement.hashing
      )
    val initial = Vector.newBuilder[WLColour]
    nodeKeys.foreach { key =>
      initial += interner.internInitial(key)
    }
    var allLevels = Vector(initial.result())
    var previous = allLevels.head
    var round = 1
    var failure: Option[WLTraceError[V]] = None

    while round <= refinement.rounds && failure.isEmpty do
      val current = Vector.newBuilder[WLColour]
      var vertexIndex = 0
      while vertexIndex < packed.vertexCount && failure.isEmpty do
        val neighbourhood =
          incident(vertexIndex).map { case (target, edgeKey, orientation) =>
            WLIncidence(edgeKey, orientation, previous(target))
          }
        interner.internRefined(
          previous(vertexIndex),
          neighbourhood
        ) match
          case Right(colour) =>
            current += colour
          case Left(error) =>
            failure = Some(
              WLTraceError.Interning(
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
