package grakern.core

import cats.kernel.Hash
import scala.collection.mutable

/** Direction of an incidence relative to the vertex it was requested from.
  *
  * `Outgoing` at `v` means the edge leaves `v` towards `other`; the same physical edge appears as
  * `Incoming` at `other`. `Undirected` edges appear with the same orientation at both endpoints.
  */
enum Orientation derives CanEqual:
  case Outgoing
  case Incoming
  case Undirected

  def reversed: Orientation =
    this match
      case Outgoing   => Incoming
      case Incoming   => Outgoing
      case Undirected => Undirected

/** One labelled incidence observed from a vertex. */
final case class Incidence[V, +E](
    other: V,
    value: E,
    orientation: Orientation
)

/** Identity of one physical edge inside a sample: canonical endpoints plus an ordinal that
  * distinguishes parallel edges with the same endpoints and orientation class.
  *
  * For `Undirected` edges `first`/`second` are the endpoints in the order they were first
  * enumerated; for directed edges `first` is the source and `second` the target.
  */
final case class EdgeRef[+V](
    first: V,
    second: V,
    orientation: Orientation,
    ordinal: Int
)

/** The topology-neutral graph input of every grakern kernel (ADR-009).
  *
  * The protocol exposes exactly what refinement and decomposition consume: graph-local vertex
  * identity, one total node value per vertex, and the labelled incidences at each vertex. Anything
  * richer belongs to an adapter, never to this trait.
  *
  * Laws (checked by `NeighbourhoodSample` and by adapter suites):
  *   - `vertices` are pairwise distinct under `vertexHash`;
  *   - every `Incidence.other` is a vertex;
  *   - for every `Undirected` incidence `v -> u` there is a matching `u -> v` with an equal value,
  *     counted with multiplicity (a self-loop is a single incidence);
  *   - for every `Outgoing` incidence `v -> u` there is exactly one matching `Incoming` incidence
  *     `u -> v` with an equal value, and vice versa.
  */
trait LabelledNeighbourhood[V, +N, +E]:
  def key: SampleKey
  def vertexHash: Hash[V]
  def vertices: IndexedSeq[V]
  def nodeValue(vertex: V): N
  def incidences(vertex: V): IndexedSeq[Incidence[V, E]]

enum NeighbourhoodError[V] derives CanEqual:
  case DuplicateVertex(sample: SampleKey, vertex: V)
  case UnknownEndpoint(sample: SampleKey, vertex: V, other: V)
  case UnmatchedIncidence(
      sample: SampleKey,
      vertex: V,
      other: V,
      orientation: Orientation
  )
  case UnknownVertex(sample: SampleKey, vertex: V)

/** A concrete, validated, immutable neighbourhood: the reference adapter for adjacency lists,
  * directed graphs, and labelled multigraphs.
  */
final class NeighbourhoodSample[V, +N, +E] private (
    val key: SampleKey,
    val vertexHash: Hash[V],
    private val vertexSeq: Vector[V],
    private val index: Map[HashKey[V], Int],
    private val values: Vector[N],
    private val incidence: Vector[Vector[Incidence[V, E]]]
) extends LabelledNeighbourhood[V, N, E]:
  def vertices: IndexedSeq[V] = vertexSeq

  def indexOf(vertex: V): Option[Int] =
    index.get(HashKey(vertex)(using vertexHash))

  def nodeValue(vertex: V): N =
    indexOf(vertex) match
      case Some(position) => values(position)
      case None           =>
        throw new NoSuchElementException(
          s"vertex $vertex is not in sample ${key.value}"
        )

  def incidences(vertex: V): IndexedSeq[Incidence[V, E]] =
    indexOf(vertex).fold(Vector.empty[Incidence[V, E]])(incidence(_))

  def vertexCount: Int = vertexSeq.size

  def incidenceCount: Int = incidence.iterator.map(_.size).sum

object NeighbourhoodSample:
  /** Validate an explicit incidence list. `incidences(v)` must satisfy the protocol laws. */
  def build[V, N, E](
      key: SampleKey,
      vertices: IndexedSeq[V],
      nodeValue: V => N,
      incidences: V => IndexedSeq[Incidence[V, E]]
  )(using
      hash: Hash[V],
      eqE: cats.kernel.Eq[E]
  ): Either[NeighbourhoodError[V], NeighbourhoodSample[V, N, E]] =
    val vertexSeq = vertices.toVector
    val indexBuilder = mutable.HashMap.empty[HashKey[V], Int]
    var failure: Option[NeighbourhoodError[V]] = None
    var position = 0
    while position < vertexSeq.size && failure.isEmpty do
      val vertex = vertexSeq(position)
      val wrapped = HashKey(vertex)
      if indexBuilder.contains(wrapped) then
        failure = Some(NeighbourhoodError.DuplicateVertex(key, vertex))
      else indexBuilder.update(wrapped, position)
      position += 1
    failure match
      case Some(error) => Left(error)
      case None        =>
        val index = indexBuilder.toMap
        val values = vertexSeq.map(nodeValue)
        val lists = vertexSeq.map(v => incidences(v).toVector)
        Neighbourhoods
          .checkLaws(key, vertexSeq, index, lists)(using hash, eqE)
          .map(_ => new NeighbourhoodSample(key, hash, vertexSeq, index, values, lists))

  /** Build from an undirected edge list; every edge contributes one incidence at each endpoint (one
    * incidence for a self-loop).
    */
  def undirected[V, N, E](
      key: SampleKey,
      vertices: IndexedSeq[V],
      nodeValue: V => N,
      edges: IterableOnce[(V, V, E)]
  )(using Hash[V], cats.kernel.Eq[E]): Either[NeighbourhoodError[V], NeighbourhoodSample[V, N, E]] =
    val lists = mutable.HashMap.empty[HashKey[V], mutable.ArrayBuffer[Incidence[V, E]]]
    def at(v: V) = lists.getOrElseUpdate(HashKey(v), mutable.ArrayBuffer.empty)
    edges.iterator.foreach { case (left, right, value) =>
      at(left) += Incidence(right, value, Orientation.Undirected)
      if !Hash[V].eqv(left, right) then at(right) += Incidence(left, value, Orientation.Undirected)
    }
    build(
      key,
      vertices,
      nodeValue,
      v => lists.get(HashKey(v)).fold(Vector.empty[Incidence[V, E]])(_.toVector)
    )

  /** Build from a directed arc list; every arc contributes an `Outgoing` incidence at its source
    * and an `Incoming` incidence at its target (a self-loop contributes both at the same vertex).
    */
  def directed[V, N, E](
      key: SampleKey,
      vertices: IndexedSeq[V],
      nodeValue: V => N,
      arcs: IterableOnce[(V, V, E)]
  )(using Hash[V], cats.kernel.Eq[E]): Either[NeighbourhoodError[V], NeighbourhoodSample[V, N, E]] =
    val lists = mutable.HashMap.empty[HashKey[V], mutable.ArrayBuffer[Incidence[V, E]]]
    def at(v: V) = lists.getOrElseUpdate(HashKey(v), mutable.ArrayBuffer.empty)
    arcs.iterator.foreach { case (source, target, value) =>
      at(source) += Incidence(target, value, Orientation.Outgoing)
      at(target) += Incidence(source, value, Orientation.Incoming)
    }
    build(
      key,
      vertices,
      nodeValue,
      v => lists.get(HashKey(v)).fold(Vector.empty[Incidence[V, E]])(_.toVector)
    )

/** A hashing wrapper so graph-local identity follows the sample's `Hash[V]`, not universal
  * equality.
  */
final class HashKey[V](val value: V)(using hash: Hash[V]):
  override def equals(other: Any): Boolean =
    other match
      case that: HashKey[?] =>
        try hash.eqv(value, that.value.asInstanceOf[V])
        catch case _: ClassCastException => false
      case _ => false

  override def hashCode(): Int = hash.hash(value)

object HashKey:
  def apply[V](value: V)(using Hash[V]): HashKey[V] = new HashKey(value)

/** One physical edge enumerated exactly once from a neighbourhood. */
final case class CanonicalEdge[V, +E](
    ref: EdgeRef[V],
    value: E
)

object Neighbourhoods:
  /** Enumerate every physical edge once, in vertex order: an `Undirected` edge is emitted from its
    * lower-indexed endpoint (or once at a self-loop), a directed edge from its source. Parallel
    * edges receive increasing ordinals per (endpoints, orientation class).
    */
  def canonicalEdges[V, N, E](
      sample: LabelledNeighbourhood[V, N, E]
  ): Vector[CanonicalEdge[V, E]] =
    given Hash[V] = sample.vertexHash
    val vertices = sample.vertices
    val index = vertices.iterator.zipWithIndex.map { case (v, i) => HashKey(v) -> i }.toMap
    val ordinals = mutable.HashMap.empty[(Int, Int, Boolean), Int]
    val out = Vector.newBuilder[CanonicalEdge[V, E]]
    var position = 0
    while position < vertices.size do
      val vertex = vertices(position)
      sample.incidences(vertex).foreach { inc =>
        val otherIndex = index.getOrElse(HashKey(inc.other), -1)
        val directed = inc.orientation != Orientation.Undirected
        val emit =
          inc.orientation match
            case Orientation.Undirected => otherIndex >= position
            case Orientation.Outgoing   => true
            case Orientation.Incoming   => false
        if emit then
          val slot = (position, otherIndex, directed)
          val ordinal = ordinals.getOrElse(slot, 0)
          ordinals.update(slot, ordinal + 1)
          out += CanonicalEdge(
            EdgeRef(vertex, inc.other, inc.orientation, ordinal),
            inc.value
          )
      }
      position += 1
    out.result()

  private[core] def checkLaws[V, E](
      key: SampleKey,
      vertices: Vector[V],
      index: Map[HashKey[V], Int],
      lists: Vector[Vector[Incidence[V, E]]]
  )(using hash: Hash[V], eqE: cats.kernel.Eq[E]): Either[NeighbourhoodError[V], Unit] =
    // Count incidences by (from, to, orientation, value) so multiplicities can be matched exactly.
    val counts = mutable.HashMap.empty[(Int, Int, Orientation), mutable.ArrayBuffer[E]]
    var position = 0
    var failure: Option[NeighbourhoodError[V]] = None
    while position < lists.size && failure.isEmpty do
      lists(position).foreach { inc =>
        index.get(HashKey(inc.other)) match
          case None =>
            if failure.isEmpty then
              failure = Some(NeighbourhoodError.UnknownEndpoint(key, vertices(position), inc.other))
          case Some(other) =>
            counts.getOrElseUpdate((position, other, inc.orientation), mutable.ArrayBuffer.empty) +=
              inc.value
      }
      position += 1
    failure match
      case Some(error) => Left(error)
      case None        =>
        // Each (v -> u, orientation) multiset must be matched by (u -> v, reversed orientation).
        val unmatched =
          counts.iterator.collectFirst {
            case ((from, to, orientation), values)
                if from != to || orientation != Orientation.Undirected =>
              val partner = counts.get((to, from, orientation.reversed))
              val matched = partner.exists(other => sameMultiset(values, other))
              Option.when(!matched)((from, to, orientation))
          }.flatten
        unmatched match
          case Some((from, to, orientation)) =>
            Left(
              NeighbourhoodError.UnmatchedIncidence(
                key,
                vertices(from),
                vertices(to),
                orientation
              )
            )
          case None => Right(())

  private def sameMultiset[E](
      left: mutable.ArrayBuffer[E],
      right: mutable.ArrayBuffer[E]
  )(using eqE: cats.kernel.Eq[E]): Boolean =
    if left.size != right.size then false
    else
      val used = new Array[Boolean](right.size)
      left.forall { value =>
        var i = 0
        var found = false
        while i < right.size && !found do
          if !used(i) && eqE.eqv(value, right(i)) then
            used(i) = true
            found = true
          i += 1
        found
      }
