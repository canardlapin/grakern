package grakern.engine

import cats.kernel.Hash
import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.wl.*
import scala.collection.mutable.ArrayBuffer

private[grakern] enum WLTraceError[V]:
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
  def prepare[
      V,
      N,
      E,
      NodeKey: Hash,
      EdgeKey: Hash
  ](
      samples: Vector[GraphSample[V, N, E]],
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
    var interner =
      WLInterner.empty(
        refinement.codecs,
        refinement.hashing
      )
    val initial = Vector.newBuilder[WLColour]
    nodeKeys.foreach { key =>
      val (next, colour) = interner.internInitial(key)
      interner = next
      initial += colour
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
          incident(vertexIndex).iterator.map { case (target, edgeKey) =>
            edgeKey -> previous(target)
          }.toVector
        interner.internRefined(
          previous(vertexIndex),
          neighbourhood
        ) match
          case Right((next, colour)) =>
            interner = next
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
        interner
      )
    )
