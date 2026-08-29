package grakern.standard.wl

import cats.kernel.Hash
import grakern.core.Orientation
import scala.util.hashing.MurmurHash3

trait WLHashStrategy[-N, -E]:
  def initial(nodeKey: N): Int

  def refined(
      level: Int,
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Int

  private[grakern] def refinedView(
      level: Int,
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Int =
    refined(level, parent, neighbourhood.toVector)

private[grakern] trait WLIncidenceView[+E]:
  def size: Int
  def edgeKey(index: Int): E
  def orientation(index: Int): Orientation
  def colour(index: Int): WLColour

  final def toVector: Vector[WLIncidence[E]] =
    Vector.tabulate(size)(index => WLIncidence(edgeKey(index), orientation(index), colour(index)))

private[wl] object WLIncidenceView:
  def fromVector[E](values: Vector[WLIncidence[E]]): WLIncidenceView[E] =
    new WLIncidenceView[E]:
      def size: Int = values.size
      def edgeKey(index: Int): E = values(index).edgeKey
      def orientation(index: Int): Orientation = values(index).orientation
      def colour(index: Int): WLColour = values(index).colour

object WLHashStrategy:
  private def orientationSalt(orientation: Orientation): Int =
    orientation match
      case Orientation.Undirected => 0
      case Orientation.Outgoing   => 0x2f1a7c3
      case Orientation.Incoming   => 0x5b8d1e9

  def default[N: Hash, E: Hash]: WLHashStrategy[N, E] =
    new WLHashStrategy[N, E]:
      def initial(nodeKey: N): Int =
        MurmurHash3.mix(0x1020304, Hash[N].hash(nodeKey))

      def refined(
          level: Int,
          parent: WLColour,
          neighbourhood: Vector[WLIncidence[E]]
      ): Int =
        var unordered = 0
        var index = 0
        while index < neighbourhood.size do
          val incidence = neighbourhood(index)
          unordered += MurmurHash3.mix(
            Hash[E].hash(incidence.edgeKey) ^ orientationSalt(incidence.orientation),
            incidence.colour.index
          )
          index += 1
        MurmurHash3.finalizeHash(
          MurmurHash3.mix(
            MurmurHash3.mix(level, parent.index),
            unordered
          ),
          neighbourhood.size
        )

      override private[grakern] def refinedView(
          level: Int,
          parent: WLColour,
          neighbourhood: WLIncidenceView[E]
      ): Int =
        var unordered = 0
        var index = 0
        while index < neighbourhood.size do
          unordered += MurmurHash3.mix(
            Hash[E].hash(neighbourhood.edgeKey(index)) ^
              orientationSalt(neighbourhood.orientation(index)),
            neighbourhood.colour(index).index
          )
          index += 1
        MurmurHash3.finalizeHash(
          MurmurHash3.mix(
            MurmurHash3.mix(level, parent.index),
            unordered
          ),
          neighbourhood.size
        )

  def constant[N, E](value: Int): WLHashStrategy[N, E] =
    new WLHashStrategy[N, E]:
      def initial(nodeKey: N): Int = value

      def refined(
          level: Int,
          parent: WLColour,
          neighbourhood: Vector[WLIncidence[E]]
      ): Int = value

      override private[grakern] def refinedView(
          level: Int,
          parent: WLColour,
          neighbourhood: WLIncidenceView[E]
      ): Int = value

enum WLInternerError:
  case ForeignColour(colour: WLColour)
  case InvalidNeighbourLevel(
      parent: WLColour,
      neighbour: WLColour
  )

/** Compact immutable hash buckets for a frozen interner.
  *
  * The hash keys are sorted. `offsets` and `nodeIndices` form a CSR-like index, avoiding an
  * immutable `Map[Int, Vector[Int]]` (and its boxed keys) on the preparation hot path.
  */
final private[wl] class WLInternerBuckets private (
    private val hashes: Array[Int],
    private val offsets: Array[Int],
    private val nodeIndices: Array[Int]
):
  def bucket(hash: Int): Int =
    var low = 0
    var high = hashes.length - 1
    while low <= high do
      val middle = low + ((high - low) >>> 1)
      val candidate = hashes(middle)
      if candidate < hash then low = middle + 1
      else if candidate > hash then high = middle - 1
      else return middle
    -(low + 1)

  def start(bucket: Int): Int = offsets(bucket)

  def end(bucket: Int): Int = offsets(bucket + 1)

  def nodeIndex(position: Int): Int = nodeIndices(position)

  def appended(hash: Int, nodeIndex: Int): WLInternerBuckets =
    val existing = bucket(hash)
    val insertionBucket = if existing >= 0 then existing else -existing - 1
    val insertionPosition = offsets(insertionBucket + (if existing >= 0 then 1 else 0))

    val updatedHashes =
      if existing >= 0 then hashes
      else
        val result = new Array[Int](hashes.length + 1)
        Array.copy(hashes, 0, result, 0, insertionBucket)
        result(insertionBucket) = hash
        Array.copy(
          hashes,
          insertionBucket,
          result,
          insertionBucket + 1,
          hashes.length - insertionBucket
        )
        result

    val updatedIndices = new Array[Int](nodeIndices.length + 1)
    Array.copy(nodeIndices, 0, updatedIndices, 0, insertionPosition)
    updatedIndices(insertionPosition) = nodeIndex
    Array.copy(
      nodeIndices,
      insertionPosition,
      updatedIndices,
      insertionPosition + 1,
      nodeIndices.length - insertionPosition
    )

    val updatedOffsets =
      if existing >= 0 then
        val result = offsets.clone()
        var index = existing + 1
        while index < result.length do
          result(index) += 1
          index += 1
        result
      else
        val result = new Array[Int](updatedHashes.length + 1)
        var index = 0
        while index < result.length do
          result(index) =
            if index <= insertionBucket then offsets(index)
            else offsets(index - 1) + 1
          index += 1
        result

    new WLInternerBuckets(updatedHashes, updatedOffsets, updatedIndices)

private[wl] object WLInternerBuckets:
  val empty: WLInternerBuckets =
    new WLInternerBuckets(Array.emptyIntArray, Array(0), Array.emptyIntArray)

  def fromArrays(
      hashes: Array[Int],
      offsets: Array[Int],
      nodeIndices: Array[Int]
  ): WLInternerBuckets =
    new WLInternerBuckets(hashes, offsets, nodeIndices)

/** An immutable, hash-consed store of exact recursive WL signatures.
  *
  * Hashes select candidate buckets only. Initial labels and refined (edge key, orientation, colour)
  * multisets are compared exactly before an existing colour is reused.
  */
final class WLInterner[N, E] private[wl] (
    private[wl] val owner: ColourOwner,
    private[wl] val nodeHash: Hash[N],
    private[wl] val edgeHash: Hash[E],
    private[wl] val hashing: WLHashStrategy[N, E],
    private[wl] val codecs: WLCodecs[N, E],
    private[wl] val nodes: Vector[WLSignature[N, E]],
    private[wl] val buckets: WLInternerBuckets
):
  def size: Int = nodes.size

  def colours: Vector[WLColour] =
    nodes.map(_.colour)

  def signature(colour: WLColour): Option[WLSignature[N, E]] =
    if owns(colour) then nodes.lift(colour.index).filter(_.level == colour.level)
    else None

  def internInitial(
      nodeKey: N
  ): (WLInterner[N, E], WLColour) =
    findInitial(nodeKey) match
      case Some(colour) => (this, colour)
      case None         =>
        val hash = hashing.initial(nodeKey)
        val colour = WLColour.create(owner, nodes.size, level = 0)
        val signature =
          WLSignature.initial[N, E](
            colour,
            nodeKey,
            WLDescriptor.initial(nodeKey, codecs.node)
          )
        (
          appended(hash, signature),
          colour
        )

  def internRefined(
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Either[WLInternerError, (WLInterner[N, E], WLColour)] =
    validateReferences(parent, neighbourhood).map { _ =>
      val level = parent.level + 1
      findRefined(level, parent, neighbourhood) match
        case Some(colour) => (this, colour)
        case None         =>
          val hash = hashing.refined(level, parent, neighbourhood)
          val colour = WLColour.create(owner, nodes.size, level)
          val neighbours =
            neighbourhood.map { incidence =>
              new WLNeighbour(incidence.edgeKey, incidence.orientation, incidence.colour)
            }
          val parentDescriptor =
            signature(parent).flatMap(_.stableDescriptor)
          val descriptor =
            if parentDescriptor.isEmpty || codecs.edge.isEmpty then None
            else
              WLDescriptor.refined(
                level,
                parentDescriptor,
                neighbourhood.map { incidence =>
                  (
                    incidence.edgeKey,
                    incidence.orientation,
                    signature(incidence.colour).flatMap(_.stableDescriptor)
                  )
                },
                codecs.edge
              )
          val value =
            WLSignature.refined[N, E](
              colour,
              parent,
              neighbours,
              descriptor
            )
          (
            appended(hash, value),
            colour
          )
    }

  def semanticallyEquivalent(
      left: WLColour,
      that: WLInterner[N, E],
      right: WLColour
  ): Boolean =
    (signature(left), that.signature(right)) match
      case (
            Some(leftInitial: WLSignature.Initial[N, E]),
            Some(rightInitial: WLSignature.Initial[N, E])
          ) =>
        nodeHash.eqv(leftInitial.nodeKey, rightInitial.nodeKey)
      case (
            Some(leftRefined: WLSignature.Refined[N, E]),
            Some(rightRefined: WLSignature.Refined[N, E])
          ) =>
        leftRefined.level == rightRefined.level &&
        semanticallyEquivalent(
          leftRefined.previous,
          that,
          rightRefined.previous
        ) &&
        exactCrossTraceMultiset(
          leftRefined.neighbourhood,
          that,
          rightRefined.neighbourhood
        )
      case _ =>
        false

  private[wl] def findInitial(nodeKey: N): Option[WLColour] =
    val hash = hashing.initial(nodeKey)
    val bucket = buckets.bucket(hash)
    if bucket < 0 then None
    else
      var position = buckets.start(bucket)
      val end = buckets.end(bucket)
      while position < end do
        nodes(buckets.nodeIndex(position)) match
          case initial: WLSignature.Initial[N, E] if nodeHash.eqv(initial.nodeKey, nodeKey) =>
            return Some(initial.colour)
          case _ =>
        position += 1
      None

  private[wl] def findRefined(
      level: Int,
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Option[WLColour] =
    findRefinedView(level, parent, WLIncidenceView.fromVector(neighbourhood))

  private[wl] def findRefinedView(
      level: Int,
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Option[WLColour] =
    val hash = hashing.refinedView(level, parent, neighbourhood)
    val bucket = buckets.bucket(hash)
    if bucket < 0 then None
    else
      var position = buckets.start(bucket)
      val end = buckets.end(bucket)
      while position < end do
        nodes(buckets.nodeIndex(position)) match
          case refined: WLSignature.Refined[N, E]
              if refined.level == level &&
                refined.previous.equals(parent) &&
                WLInterner.exactMultisetView(edgeHash, refined.neighbourhood, neighbourhood) =>
            return Some(refined.colour)
          case _ =>
        position += 1
      None

  private[wl] def owns(colour: WLColour): Boolean =
    colour.owner.eq(owner)

  private def validateReferences(
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Either[WLInternerError, Unit] =
    if !owns(parent) then Left(WLInternerError.ForeignColour(parent))
    else
      neighbourhood
        .collectFirst {
          case incidence if !owns(incidence.colour) =>
            WLInternerError.ForeignColour(incidence.colour)
          case incidence if incidence.colour.level != parent.level =>
            WLInternerError.InvalidNeighbourLevel(parent, incidence.colour)
        }
        .toLeft(())

  private def appended(
      hash: Int,
      signature: WLSignature[N, E]
  ): WLInterner[N, E] =
    new WLInterner(
      owner,
      nodeHash,
      edgeHash,
      hashing,
      codecs,
      nodes :+ signature,
      buckets.appended(hash, nodes.size)
    )

  private def exactCrossTraceMultiset(
      left: Vector[WLNeighbour[E]],
      that: WLInterner[N, E],
      right: Vector[WLNeighbour[E]]
  ): Boolean =
    if left.size != right.size then false
    else
      val matched = Array.fill(right.size)(false)
      left.forall { expected =>
        val index =
          right.indices.find { candidate =>
            !matched(candidate) &&
            edgeHash.eqv(
              expected.edgeKey,
              right(candidate).edgeKey
            ) &&
            expected.orientation == right(candidate).orientation &&
            semanticallyEquivalent(
              expected.colour,
              that,
              right(candidate).colour
            )
          }
        index.foreach(candidate => matched(candidate) = true)
        index.nonEmpty
      }

object WLInterner:
  def empty[N: Hash, E: Hash](
      codecs: WLCodecs[N, E]
  ): WLInterner[N, E] =
    empty(codecs, WLHashStrategy.default[N, E])

  def empty[N: Hash, E: Hash](
      codecs: WLCodecs[N, E],
      hashing: WLHashStrategy[N, E]
  ): WLInterner[N, E] =
    new WLInterner(
      new ColourOwner,
      Hash[N],
      Hash[E],
      hashing,
      codecs,
      Vector.empty,
      WLInternerBuckets.empty
    )

  /** Exact multiset equality of a stored neighbourhood against an incoming one: edge key under
    * `Hash[E]`, orientation, and colour must all match with multiplicity.
    */
  private[wl] def exactMultiset[E](
      edgeHash: Hash[E],
      stored: Vector[WLNeighbour[E]],
      incoming: Vector[WLIncidence[E]]
  ): Boolean =
    exactMultisetView(edgeHash, stored, WLIncidenceView.fromVector(incoming))

  private[wl] def exactMultisetView[E](
      edgeHash: Hash[E],
      stored: Vector[WLNeighbour[E]],
      incoming: WLIncidenceView[E]
  ): Boolean =
    if stored.size != incoming.size then false
    else
      val matched = Array.fill(incoming.size)(false)
      var storedIndex = 0
      while storedIndex < stored.size do
        val expected = stored(storedIndex)
        var candidate = 0
        var found = -1
        while candidate < incoming.size && found < 0 do
          if !matched(candidate) &&
            edgeHash.eqv(
              expected.edgeKey,
              incoming.edgeKey(candidate)
            ) &&
            expected.orientation == incoming.orientation(candidate) &&
            expected.colour.equals(incoming.colour(candidate))
          then found = candidate
          candidate += 1
        if found < 0 then return false
        matched(found) = true
        storedIndex += 1
      true
