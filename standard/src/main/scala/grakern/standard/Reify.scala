package grakern.standard

import cats.kernel.{Eq, Hash}
import grakern.core.*
import scala.util.hashing.MurmurHash3

/** Vertex identity in the relation-as-vertex construction. */
enum ReifiedVertex[+V] derives CanEqual:
  case Entity(value: V)
  case Relation(edge: EdgeRef[V])

/** Node value in the relation-as-vertex construction. */
enum ReifiedNode[+N, +E] derives CanEqual:
  case Entity(value: N)
  case Relation(value: E)

/** Role of an entity endpoint relative to a reified relation node. */
enum ReifiedEndpoint derives CanEqual:
  case Undirected
  case Source
  case Target

enum ReificationError[V] derives CanEqual:
  case InvalidOutput(error: NeighbourhoodError[ReifiedVertex[V]])

object Reify:
  /** Turn every physical labelled edge into a node in a deterministic bipartite neighbourhood.
    *
    * An undirected relation connects to each distinct endpoint with `Undirected`; a self-loop has
    * one such connection. A directed relation connects to its source and target with distinct
    * endpoint roles, including two labelled parallel connections for a directed self-loop. Parallel
    * input relations remain distinct through their [[EdgeRef.ordinal]] and relation-node value.
    */
  def relationsAsVertices[V, N, E](
      sample: LabelledNeighbourhood[V, N, E]
  ): Either[
    ReificationError[V],
    NeighbourhoodSample[ReifiedVertex[V], ReifiedNode[N, E], ReifiedEndpoint]
  ] =
    given Hash[V] = sample.vertexHash
    given Hash[ReifiedVertex[V]] = reifiedVertexHash
    given Eq[ReifiedEndpoint] = Eq.fromUniversalEquals

    val entities = sample.vertices.toVector.map(ReifiedVertex.Entity(_))
    val relations = Neighbourhoods.canonicalEdges(sample)
    val relationVertices = relations.map(edge => ReifiedVertex.Relation(edge.ref))
    val nodeValues =
      (entities.zip(sample.vertices.toVector.map(v => ReifiedNode.Entity(sample.nodeValue(v)))) ++
        relationVertices.zip(relations.map(edge => ReifiedNode.Relation(edge.value)))).map {
        case (vertex, value) => HashKey(vertex) -> value
      }.toMap
    val edges = relations.flatMap { relation =>
      val relationVertex = ReifiedVertex.Relation(relation.ref)
      val first = ReifiedVertex.Entity(relation.ref.first)
      val second = ReifiedVertex.Entity(relation.ref.second)
      relation.ref.orientation match
        case Orientation.Undirected =>
          if sample.vertexHash.eqv(relation.ref.first, relation.ref.second) then
            Vector((relationVertex, first, ReifiedEndpoint.Undirected))
          else
            Vector(
              (relationVertex, first, ReifiedEndpoint.Undirected),
              (relationVertex, second, ReifiedEndpoint.Undirected)
            )
        case Orientation.Outgoing =>
          Vector(
            (relationVertex, first, ReifiedEndpoint.Source),
            (relationVertex, second, ReifiedEndpoint.Target)
          )
        case Orientation.Incoming =>
          // canonicalEdges emits directed relations from their Outgoing incidence.
          Vector.empty
    }
    NeighbourhoodSample
      .undirected(
        sample.key,
        entities ++ relationVertices,
        vertex => nodeValues(HashKey(vertex)),
        edges
      )
      .left
      .map(ReificationError.InvalidOutput(_))

  private def reifiedVertexHash[V](using vertexHash: Hash[V]): Hash[ReifiedVertex[V]] =
    new Hash[ReifiedVertex[V]]:
      def eqv(left: ReifiedVertex[V], right: ReifiedVertex[V]): Boolean =
        (left, right) match
          case (ReifiedVertex.Entity(a), ReifiedVertex.Entity(b))     => vertexHash.eqv(a, b)
          case (ReifiedVertex.Relation(a), ReifiedVertex.Relation(b)) =>
            vertexHash.eqv(a.first, b.first) &&
            vertexHash.eqv(a.second, b.second) &&
            a.orientation == b.orientation &&
            a.ordinal == b.ordinal
          case _ => false

      def hash(value: ReifiedVertex[V]): Int =
        value match
          case ReifiedVertex.Entity(entity) =>
            MurmurHash3.mix(0x13a9d2f1, vertexHash.hash(entity))
          case ReifiedVertex.Relation(edge) =>
            val orientation =
              edge.orientation match
                case Orientation.Undirected => 0
                case Orientation.Outgoing   => 1
                case Orientation.Incoming   => 2
            MurmurHash3.finalizeHash(
              MurmurHash3.mix(
                MurmurHash3.mix(
                  MurmurHash3.mix(0x62f4c8b7, vertexHash.hash(edge.first)),
                  vertexHash.hash(edge.second)
                ),
                31 * orientation + edge.ordinal
              ),
              4
            )
