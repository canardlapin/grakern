package grakern.graph4s

import cats.kernel.{Eq, Hash}
import grakern.core.{Incidence, LabelledNeighbourhood, Orientation, SampleKey}
import graph4s.{Edge, Graph}
import graph4s.data.{EdgeField, VertexField}

/** The graph4s adapter of the labelled-neighbourhood protocol (ADR-009).
  *
  * A graph4s `Graph` is finite, simple, loopless, and undirected, so every edge yields one
  * `Undirected` incidence at each endpoint. Node and edge fields are total by construction in
  * graph4s (they can only be built over their own topology), which is what makes `nodeValue` total.
  */
final class GraphSample[V, +N, +E] private (
    val key: SampleKey,
    val topology: Graph[V],
    val nodes: VertexField[V, N],
    val edges: EdgeField[V, E]
) extends LabelledNeighbourhood[V, N, E]:
  private val vertexSeq: Vector[V] =
    topology.vertices.iterator.toVector

  def vertexHash: Hash[V] = topology.vertexHash

  def vertices: IndexedSeq[V] = vertexSeq

  def nodeValue(vertex: V): N =
    nodes.get(vertex) match
      case Some(value) => value
      case None        =>
        throw new NoSuchElementException(
          s"vertex $vertex is not in sample ${key.value}"
        )

  def nodeValueOption(vertex: V): Option[N] = nodes.get(vertex)

  def edgeValue(edge: Edge[V]): Option[E] = edges.get(edge)

  def edgeValue(left: V, right: V): Option[E] = edges.get(left, right)

  def incidences(vertex: V): IndexedSeq[Incidence[V, E]] =
    topology.neighborsOf(vertex) match
      case None            => Vector.empty
      case Some(neighbors) =>
        neighbors.iterator.map { other =>
          edges.get(vertex, other) match
            case Some(value) => Incidence(other, value, Orientation.Undirected)
            case None        =>
              throw new NoSuchElementException(
                s"edge $vertex-$other has no value in sample ${key.value}"
              )
        }.toVector

object GraphSample:
  def build[V, N, E](
      key: SampleKey,
      topology: Graph[V]
  )(
      nodeValue: V => N,
      edgeValue: Edge[V] => E
  ): GraphSample[V, N, E] =
    new GraphSample(
      key,
      topology,
      VertexField.total(topology)(nodeValue),
      EdgeField.total(topology)(edgeValue)
    )

  def fromFields[V, N, E](
      key: SampleKey,
      topology: Graph[V],
      nodes: VertexField[V, N],
      edges: EdgeField[V, E]
  ): Either[GraphSampleError[V], GraphSample[V, N, E]] =
    if !Eq[Graph[V]].eqv(topology, nodes.topology) then
      Left(
        GraphSampleError.NodeFieldTopologyMismatch(
          key,
          topology,
          nodes.topology
        )
      )
    else if !Eq[Graph[V]].eqv(topology, edges.topology) then
      Left(
        GraphSampleError.EdgeFieldTopologyMismatch(
          key,
          topology,
          edges.topology
        )
      )
    else Right(new GraphSample(key, topology, nodes, edges))

enum GraphSampleError[V]:
  case NodeFieldTopologyMismatch(
      key: SampleKey,
      expected: Graph[V],
      actual: Graph[V]
  )
  case EdgeFieldTopologyMismatch(
      key: SampleKey,
      expected: Graph[V],
      actual: Graph[V]
  )
