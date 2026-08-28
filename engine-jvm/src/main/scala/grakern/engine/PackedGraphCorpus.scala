package grakern.engine

import grakern.core.*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

final private[grakern] case class PackedEdge[V, +E](
    source: Int,
    target: Int,
    owner: Int,
    original: EdgeRef[V],
    value: E,
    orientation: Orientation
)

final private[grakern] class NumericNodeColumn private[engine] (
    private[engine] val values: Array[Double]
):
  def size: Int = values.length
  def apply(index: Int): Double = values(index)

final private[grakern] class NonNegativeEdgeColumn private[engine] (
    private[engine] val values: Array[Double]
):
  def size: Int = values.length
  def apply(index: Int): Double = values(index)

/** Private engine state: every sample of a corpus packed into disjoint integer offsets with CSR
  * adjacency. Adjacency lists preserve the adapter's incidence order and record, per entry, the
  * packed edge and the orientation of that edge relative to the row vertex.
  */
final private[grakern] class PackedGraphCorpus[V, +N, +E] private (
    val sampleKeys: Vector[SampleKey],
    private[engine] val graphVertexOffsets: Array[Int],
    private[engine] val adjacencyOffsets: Array[Int],
    private[engine] val adjacencyTargets: Array[Int],
    private[engine] val adjacencyEdges: Array[Int],
    private[engine] val adjacencyOrientations: Array[Byte],
    private[engine] val vertexOwners: Array[Int],
    private[engine] val originalVertices: Vector[V],
    private[engine] val nodeValues: Vector[N],
    private[engine] val packedEdges: Vector[PackedEdge[V, E]]
):
  def graphCount: Int = sampleKeys.size
  def vertexCount: Int = originalVertices.size
  def edgeCount: Int = packedEdges.size

  def graphVertexRange(graphIndex: Int): Range =
    graphVertexOffsets(graphIndex) until graphVertexOffsets(graphIndex + 1)

  def neighbors(vertexIndex: Int): collection.IndexedSeqView[Int] =
    adjacencyTargets.view.slice(
      adjacencyOffsets(vertexIndex),
      adjacencyOffsets(vertexIndex + 1)
    )

  private[engine] def adjacencyStart(vertexIndex: Int): Int =
    adjacencyOffsets(vertexIndex)

  private[engine] def adjacencyEnd(vertexIndex: Int): Int =
    adjacencyOffsets(vertexIndex + 1)

  private[engine] def adjacencyTarget(adjacencyIndex: Int): Int =
    adjacencyTargets(adjacencyIndex)

  private[engine] def adjacencyEdge(adjacencyIndex: Int): Int =
    adjacencyEdges(adjacencyIndex)

  private[engine] def adjacencyOrientation(adjacencyIndex: Int): Orientation =
    PackedGraphCorpus.orientationOf(adjacencyOrientations(adjacencyIndex))

  def ownerOfVertex(vertexIndex: Int): SampleKey =
    sampleKeys(vertexOwners(vertexIndex))

  def originalVertex(vertexIndex: Int): V =
    originalVertices(vertexIndex)

  def categoricalNodeColumn[C](projection: N => C): Vector[C] =
    nodeValues.map(projection)

  def numericNodeColumn(
      descriptor: ProjectionDescriptor
  )(projection: N => Double): Either[PrepareError[V, EdgeRef[V]], NumericNodeColumn] =
    val output = new Array[Double](nodeValues.size)
    var index = 0
    var failure: Option[PrepareError[V, EdgeRef[V]]] = None
    while index < nodeValues.size && failure.isEmpty do
      val observed = projection(nodeValues(index))
      FiniteDouble.from(observed) match
        case Right(value) =>
          output(index) = value.toDouble
        case Left(reason) =>
          failure = Some(
            PrepareError.InvalidNumeric(
              SourceRef.Node(
                ownerOfVertex(index),
                originalVertices(index)
              ),
              descriptor,
              observed,
              reason
            )
          )
      index += 1
    failure.toLeft(new NumericNodeColumn(output))

  def nonNegativeEdgeColumn(
      descriptor: ProjectionDescriptor
  )(
      projection: E => Double
  ): Either[
    PrepareError[Nothing, EdgeRef[V]],
    NonNegativeEdgeColumn
  ] =
    val output = new Array[Double](packedEdges.size)
    var index = 0
    var failure: Option[PrepareError[Nothing, EdgeRef[V]]] = None
    while index < packedEdges.size && failure.isEmpty do
      val edge = packedEdges(index)
      val observed = projection(edge.value)
      NonNegativeFinite.from(observed) match
        case Right(value) =>
          output(index) = value.toDouble
        case Left(reason) =>
          failure = Some(
            PrepareError.InvalidNumeric(
              SourceRef.Edge(
                sampleKeys(edge.owner),
                edge.original
              ),
              descriptor,
              observed,
              reason
            )
          )
      index += 1
    failure.toLeft(new NonNegativeEdgeColumn(output))

private[grakern] object PackedGraphCorpus:
  private[engine] def orientationByte(orientation: Orientation): Byte =
    orientation match
      case Orientation.Undirected => 0
      case Orientation.Outgoing   => 1
      case Orientation.Incoming   => 2

  private[engine] def orientationOf(value: Byte): Orientation =
    value match
      case 1 => Orientation.Outgoing
      case 2 => Orientation.Incoming
      case _ => Orientation.Undirected

  /** Slot identity used to pair the two appearances of one physical edge. */
  final private case class Slot(lower: Int, upper: Int, directed: Boolean)

  def pack[V, N, E](
      samples: Vector[LabelledNeighbourhood[V, N, E]]
  ): Either[PackedCorpusError[V], PackedGraphCorpus[V, N, E]] =
    val sampleKeys = samples.map(_.key)
    val graphOffsets = ArrayBuffer(0)
    val adjacencyOffsets = ArrayBuffer(0)
    val adjacencyTargets = ArrayBuffer.empty[Int]
    val adjacencyEdges = ArrayBuffer.empty[Int]
    val adjacencyOrientations = ArrayBuffer.empty[Byte]
    val vertexOwners = ArrayBuffer.empty[Int]
    val originalVertices = Vector.newBuilder[V]
    val nodeValues = Vector.newBuilder[N]
    val packedEdges = Vector.newBuilder[PackedEdge[V, E]]
    var totalVertices = 0
    var totalEdges = 0
    var failure: Option[PackedCorpusError[V]] = None
    var graphIndex = 0

    while graphIndex < samples.size && failure.isEmpty do
      val sample = samples(graphIndex)
      given cats.kernel.Hash[V] = sample.vertexHash
      val vertices = sample.vertices
      val nextTotal = totalVertices.toLong + vertices.size.toLong
      if nextTotal > Int.MaxValue.toLong then
        failure = Some(
          PackedCorpusError.CapacityExceeded(
            sample.key,
            "vertices",
            nextTotal
          )
        )
      else
        val localToGlobal =
          vertices.iterator.zipWithIndex.map { case (vertex, local) =>
            HashKey(vertex) -> (totalVertices + local)
          }.toMap

        // Pass 1: create one packed edge per physical edge, in vertex/incidence order, and remember
        // the packed indices per slot so pass 2 can hand both endpoints the same edge.
        val created = mutable.HashMap.empty[Slot, ArrayBuffer[Int]]
        val ordinals = mutable.HashMap.empty[Slot, Int]
        var localIndex = 0
        while localIndex < vertices.size && failure.isEmpty do
          val vertex = vertices(localIndex)
          val source = totalVertices + localIndex
          sample.incidences(vertex).foreach { incidence =>
            localToGlobal.get(HashKey(incidence.other)) match
              case None =>
                if failure.isEmpty then
                  failure = Some(
                    PackedCorpusError.UnknownNeighbor(sample.key, vertex, incidence.other)
                  )
              case Some(target) =>
                val creates =
                  incidence.orientation match
                    case Orientation.Undirected => source <= target
                    case Orientation.Outgoing   => true
                    case Orientation.Incoming   => false
                if creates then
                  val slot =
                    incidence.orientation match
                      case Orientation.Undirected =>
                        Slot(math.min(source, target), math.max(source, target), false)
                      case _ => Slot(source, target, true)
                  val ordinal = ordinals.getOrElse(slot, 0)
                  ordinals.update(slot, ordinal + 1)
                  packedEdges += PackedEdge(
                    source,
                    target,
                    graphIndex,
                    EdgeRef(vertex, incidence.other, incidence.orientation, ordinal),
                    incidence.value,
                    incidence.orientation
                  )
                  created.getOrElseUpdate(slot, ArrayBuffer.empty) += totalEdges
                  totalEdges += 1
          }
          localIndex += 1

        // Pass 2: adjacency in incidence order; each endpoint side consumes the slot's edges in
        // creation order, so the k-th incidence on one side pairs with the k-th on the other.
        val cursors = mutable.HashMap.empty[(Slot, Boolean), Int]
        localIndex = 0
        while localIndex < vertices.size && failure.isEmpty do
          val vertex = vertices(localIndex)
          val source = totalVertices + localIndex
          originalVertices += vertex
          vertexOwners += graphIndex
          nodeValues += sample.nodeValue(vertex)
          sample.incidences(vertex).foreach { incidence =>
            if failure.isEmpty then
              val target = localToGlobal(HashKey(incidence.other))
              val (slot, side) =
                incidence.orientation match
                  case Orientation.Undirected =>
                    (
                      Slot(math.min(source, target), math.max(source, target), false),
                      source <= target
                    )
                  case Orientation.Outgoing => (Slot(source, target, true), true)
                  case Orientation.Incoming => (Slot(target, source, true), false)
              val cursorKey = (slot, side)
              val cursor = cursors.getOrElse(cursorKey, 0)
              created.get(slot).flatMap(_.lift(cursor)) match
                case Some(edgeIndex) =>
                  cursors.update(cursorKey, cursor + 1)
                  adjacencyTargets += target
                  adjacencyEdges += edgeIndex
                  adjacencyOrientations += orientationByte(incidence.orientation)
                case None =>
                  failure = Some(
                    PackedCorpusError.UnmatchedIncidence(
                      sample.key,
                      vertex,
                      incidence.other,
                      incidence.orientation
                    )
                  )
          }
          adjacencyOffsets += adjacencyTargets.size
          localIndex += 1

        // Every created edge must have been consumed exactly once per endpoint side.
        if failure.isEmpty then
          created.iterator
            .collectFirst {
              case (slot, edges)
                  if cursors.getOrElse((slot, true), 0) != edges.size ||
                    (slot.lower != slot.upper || slot.directed) &&
                    cursors.getOrElse((slot, false), 0) != edges.size =>
                slot
            }
            .foreach { slot =>
              failure = Some(
                PackedCorpusError.UnmatchedIncidence(
                  sample.key,
                  originalVertices.result()(slot.lower),
                  originalVertices.result()(slot.upper),
                  if slot.directed then Orientation.Outgoing else Orientation.Undirected
                )
              )
            }

        totalVertices = nextTotal.toInt
        graphOffsets += totalVertices
      graphIndex += 1

    failure.toLeft(
      new PackedGraphCorpus(
        sampleKeys,
        graphOffsets.toArray,
        adjacencyOffsets.toArray,
        adjacencyTargets.toArray,
        adjacencyEdges.toArray,
        adjacencyOrientations.toArray,
        vertexOwners.toArray,
        originalVertices.result(),
        nodeValues.result(),
        packedEdges.result()
      )
    )

enum PackedCorpusError[V]:
  case CapacityExceeded(sample: SampleKey, resource: String, requested: Long)
  case UnknownNeighbor(sample: SampleKey, source: V, target: V)
  case UnmatchedIncidence(
      sample: SampleKey,
      source: V,
      target: V,
      orientation: Orientation
  )
