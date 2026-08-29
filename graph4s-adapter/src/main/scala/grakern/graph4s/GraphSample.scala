package grakern.graph4s

import cats.kernel.{Eq, Hash}
import grakern.core.{HashKey, Incidence, LabelledNeighbourhood, Orientation, SampleKey}
import graph4s.{Edge, Graph, GraphBuildError, Link}
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

  private val incidenceIndex: Map[HashKey[V], Vector[Incidence[V, E]]] =
    given Hash[V] = topology.vertexHash
    vertexSeq.iterator.map { vertex =>
      val incidences =
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
      HashKey(vertex) -> incidences
    }.toMap

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
    given Hash[V] = topology.vertexHash
    incidenceIndex.getOrElse(HashKey(vertex), Vector.empty)

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

  /** Build the graph4s adapter from an explicit simple edge list without silently coalescing data.
    *
    * graph4s graphs are loopless and have set-valued edges. This constructor validates those rules
    * before materialization, returning a typed error for loops, parallel/reversed duplicate edges,
    * unknown endpoints, or duplicate vertices. Use `NeighbourhoodSample` for native multigraphs or
    * `Reify.relationsAsVertices` when edge values should participate as nodes.
    */
  def fromEdgeList[V: Hash, N, E](
      key: SampleKey,
      vertices: IndexedSeq[V],
      relations: Vector[GraphSampleEdge[V, E]]
  )(
      nodeValue: V => N
  ): Either[GraphSampleEdgeListError[V], GraphSample[V, N, E]] =
    val vertexHash = Hash[V]
    val duplicateVertex =
      vertices.indices.collectFirst {
        case index
            if vertices.take(index).exists(previous => vertexHash.eqv(previous, vertices(index))) =>
          GraphSampleEdgeListError.DuplicateVertex(vertices(index))
      }
    val invalidRelation =
      relations.indices.collectFirst(Function.unlift { index =>
        val relation = relations(index)
        val unknown =
          Vector(relation.left, relation.right).find(endpoint =>
            !vertices.exists(vertex => vertexHash.eqv(vertex, endpoint))
          )
        unknown
          .map(GraphSampleEdgeListError.UnknownEndpoint(index, _))
          .orElse(
            Option.when(vertexHash.eqv(relation.left, relation.right))(
              GraphSampleEdgeListError.SelfLoopUnsupported(index, relation.left)
            )
          )
          .orElse {
            relations.take(index).zipWithIndex.collectFirst {
              case (previous, previousIndex)
                  if sameUndirectedEndpoints(previous, relation)(using vertexHash) =>
                GraphSampleEdgeListError.ParallelEdgeUnsupported(
                  index,
                  previousIndex,
                  relation.left,
                  relation.right
                )
            }
          }
      })

    duplicateVertex.orElse(invalidRelation) match
      case Some(error) => Left(error)
      case None        =>
        Graph
          .of(
            vertices,
            relations.map(relation => Link(relation.left, relation.right))
          )
          .toEither
          .left
          .map(errors => GraphSampleEdgeListError.Graph4sRejected(errors.toChain.toList.toVector))
          .flatMap { topology =>
            val missing = topology.edges.iterator.find(edge =>
              !relations
                .exists(relation => sameUndirectedEndpoints(relation, edge)(using vertexHash))
            )
            missing match
              case Some(edge) =>
                Left(
                  GraphSampleEdgeListError.AdapterInvariantViolation(
                    edge.first,
                    edge.second
                  )
                )
              case None =>
                Right(
                  build(key, topology)(
                    nodeValue,
                    edge =>
                      relations.collectFirst {
                        case relation
                            if sameUndirectedEndpoints(relation, edge)(using vertexHash) =>
                          relation.value
                      }.get
                  )
                )
          }

  private def sameUndirectedEndpoints[V, E](
      left: GraphSampleEdge[V, E],
      right: GraphSampleEdge[V, ?]
  )(using hash: Hash[V]): Boolean =
    (hash.eqv(left.left, right.left) && hash.eqv(left.right, right.right)) ||
      (hash.eqv(left.left, right.right) && hash.eqv(left.right, right.left))

  private def sameUndirectedEndpoints[V, E](
      relation: GraphSampleEdge[V, E],
      edge: Edge[V]
  )(using hash: Hash[V]): Boolean =
    (hash.eqv(relation.left, edge.first) && hash.eqv(relation.right, edge.second)) ||
      (hash.eqv(relation.left, edge.second) && hash.eqv(relation.right, edge.first))

final case class GraphSampleEdge[V, +E](
    left: V,
    right: V,
    value: E
)

enum GraphSampleEdgeListError[V]:
  case DuplicateVertex(vertex: V)
  case UnknownEndpoint(edgeIndex: Int, endpoint: V)
  case SelfLoopUnsupported(edgeIndex: Int, vertex: V)
  case ParallelEdgeUnsupported(
      edgeIndex: Int,
      previousEdgeIndex: Int,
      left: V,
      right: V
  )
  case Graph4sRejected(errors: Vector[GraphBuildError[V]])
  case AdapterInvariantViolation(left: V, right: V)

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
