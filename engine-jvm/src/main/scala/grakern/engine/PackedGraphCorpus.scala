package grakern.engine

import cats.collections.HashMap
import grakern.core.*
import grakern.graph4s.GraphSample
import graph4s.Edge
import scala.collection.mutable.ArrayBuffer

final private[grakern] case class PackedEdge[V, +E](
    source: Int,
    target: Int,
    owner: Int,
    original: Edge[V],
    value: E
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

final private[grakern] class PackedGraphCorpus[V, +N, +E] private (
    val sampleKeys: Vector[SampleKey],
    private[engine] val graphVertexOffsets: Array[Int],
    private[engine] val adjacencyOffsets: Array[Int],
    private[engine] val adjacencyTargets: Array[Int],
    private[engine] val adjacencyEdges: Array[Int],
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

  def ownerOfVertex(vertexIndex: Int): SampleKey =
    sampleKeys(vertexOwners(vertexIndex))

  def originalVertex(vertexIndex: Int): V =
    originalVertices(vertexIndex)

  def categoricalNodeColumn[C](projection: N => C): Vector[C] =
    nodeValues.map(projection)

  def numericNodeColumn(
      descriptor: ProjectionDescriptor
  )(projection: N => Double): Either[PrepareError[V, Edge[V]], NumericNodeColumn] =
    val output = new Array[Double](nodeValues.size)
    var index = 0
    var failure: Option[PrepareError[V, Edge[V]]] = None
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
    PrepareError[Nothing, Edge[V]],
    NonNegativeEdgeColumn
  ] =
    val output = new Array[Double](packedEdges.size)
    var index = 0
    var failure: Option[PrepareError[Nothing, Edge[V]]] = None
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
  def pack[V, N, E](
      samples: Vector[GraphSample[V, N, E]]
  ): Either[PackedCorpusError[V], PackedGraphCorpus[V, N, E]] =
    val sampleKeys = samples.map(_.key)
    val graphOffsets = ArrayBuffer(0)
    val adjacencyOffsets = ArrayBuffer(0)
    val adjacencyTargets = ArrayBuffer.empty[Int]
    val adjacencyEdges = ArrayBuffer.empty[Int]
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
      val vertices = sample.topology.vertices.iterator.toVector
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
          HashMap.fromIterableOnce(
            vertices.iterator.zipWithIndex.map { case (vertex, local) =>
              (vertex, totalVertices + local)
            }
          )(using sample.topology.vertexHash)

        val edgeIndexByEndpoints =
          scala.collection.mutable.LongMap.empty[Int]
        sample.topology.edges.iterator.foreach { edge =>
          val endpoints =
            for
              source <- localToGlobal.get(edge.first)
              target <- localToGlobal.get(edge.second)
              value <- sample.edgeValue(edge)
            yield (source, target, value)
          endpoints match
            case Some((source, target, value)) =>
              packedEdges +=
                PackedEdge(
                  source,
                  target,
                  graphIndex,
                  edge,
                  value
                )
              edgeIndexByEndpoints.update(
                endpointKey(source, target),
                totalEdges
              )
              totalEdges += 1
            case None =>
              failure = Some(
                PackedCorpusError.MissingEdgeValue(sample.key, edge)
              )
        }

        var localIndex = 0
        while localIndex < vertices.size && failure.isEmpty do
          val vertex = vertices(localIndex)
          originalVertices += vertex
          vertexOwners += graphIndex
          sample.nodeValue(vertex) match
            case Some(value) =>
              nodeValues += value
            case None =>
              failure = Some(
                PackedCorpusError.MissingNodeValue(sample.key, vertex)
              )

          val neighbors =
            sample.topology
              .neighborsOf(vertex)
              .fold(Iterator.empty)(_.iterator)
          neighbors.foreach { neighbor =>
            localToGlobal.get(neighbor) match
              case Some(target) =>
                edgeIndexByEndpoints.get(
                  endpointKey(
                    totalVertices + localIndex,
                    target
                  )
                ) match
                  case Some(edgeIndex) =>
                    adjacencyTargets += target
                    adjacencyEdges += edgeIndex
                  case None =>
                    failure = Some(
                      PackedCorpusError.UnknownAdjacencyEdge(
                        sample.key,
                        vertex,
                        neighbor
                      )
                    )
              case None =>
                failure = Some(
                  PackedCorpusError.UnknownNeighbor(
                    sample.key,
                    vertex,
                    neighbor
                  )
                )
          }
          adjacencyOffsets += adjacencyTargets.size
          localIndex += 1

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
        vertexOwners.toArray,
        originalVertices.result(),
        nodeValues.result(),
        packedEdges.result()
      )
    )

  private def endpointKey(left: Int, right: Int): Long =
    val lower = math.min(left, right)
    val upper = math.max(left, right)
    (lower.toLong << 32) | (upper.toLong & 0xffffffffL)

enum PackedCorpusError[V]:
  case CapacityExceeded(sample: SampleKey, resource: String, requested: Long)
  case MissingNodeValue(sample: SampleKey, vertex: V)
  case MissingEdgeValue(sample: SampleKey, edge: Edge[V])
  case UnknownNeighbor(sample: SampleKey, source: V, target: V)
  case UnknownAdjacencyEdge(sample: SampleKey, source: V, target: V)
