package grakern.standard.wl

import cats.kernel.Hash
import scala.collection.mutable

/** Preparation-local primitive hash chains.
  *
  * One open-addressed table maps an exact `Int` hash to the head and tail of a node-index chain.
  * Candidate indices and links remain primitive throughout refinement.
  */
final private[wl] class MutableWLInternerBuckets(expectedNodes: Int):
  private val initialTableCapacity =
    // A preparation knows a safe upper bound on its signatures. Pre-sizing avoids repeatedly
    // rehashing the complete table while retaining a conservative cap for very large corpora.
    val desired = math.min(
      1L << 20,
      math.max(16L, (math.max(0, expectedNodes).toLong * 10L) / 7L + 1L)
    )
    var capacity = 16
    while capacity.toLong < desired do capacity = capacity << 1
    capacity
  private val initialNodeCapacity =
    math.max(16, math.min(1 << 20, math.max(0, expectedNodes)))
  private var hashes = new Array[Int](initialTableCapacity)
  private var occupied = new Array[Boolean](initialTableCapacity)
  private var heads = Array.fill(initialTableCapacity)(-1)
  private var tails = Array.fill(initialTableCapacity)(-1)
  private var links = Array.fill(initialNodeCapacity)(-1)
  private var bucketCount = 0

  def head(hash: Int): Int =
    val slot = findSlot(hash)
    if occupied(slot) then heads(slot) else -1

  def nextIndex(nodeIndex: Int): Int = links(nodeIndex)

  def append(hash: Int, nodeIndex: Int): Unit =
    ensureNodeCapacity(nodeIndex + 1)
    var slot = findSlot(hash)
    if !occupied(slot) && (bucketCount + 1) * 10 >= hashes.length * 7 then
      growTable()
      slot = findSlot(hash)

    links(nodeIndex) = -1
    if occupied(slot) then
      links(tails(slot)) = nodeIndex
      tails(slot) = nodeIndex
    else
      occupied(slot) = true
      hashes(slot) = hash
      heads(slot) = nodeIndex
      tails(slot) = nodeIndex
      bucketCount += 1

  def freeze(): WLInternerBuckets =
    if bucketCount == 0 then WLInternerBuckets.empty
    else
      val sortedHashes = new Array[Int](bucketCount)
      var slot = 0
      var hashIndex = 0
      while slot < hashes.length do
        if occupied(slot) then
          sortedHashes(hashIndex) = hashes(slot)
          hashIndex += 1
        slot += 1
      scala.util.Sorting.quickSort(sortedHashes)

      val offsets = new Array[Int](bucketCount + 1)
      hashIndex = 0
      while hashIndex < sortedHashes.length do
        var count = 0
        var node = head(sortedHashes(hashIndex))
        while node >= 0 do
          count += 1
          node = links(node)
        offsets(hashIndex + 1) = offsets(hashIndex) + count
        hashIndex += 1

      val nodeIndices = new Array[Int](offsets.last)
      hashIndex = 0
      while hashIndex < sortedHashes.length do
        var destination = offsets(hashIndex)
        var node = head(sortedHashes(hashIndex))
        while node >= 0 do
          nodeIndices(destination) = node
          destination += 1
          node = links(node)
        hashIndex += 1
      WLInternerBuckets.fromArrays(sortedHashes, offsets, nodeIndices)

  private def findSlot(hash: Int): Int =
    val mask = hashes.length - 1
    var slot = spread(hash) & mask
    while occupied(slot) && hashes(slot) != hash do slot = (slot + 1) & mask
    slot

  private def growTable(): Unit =
    val previousHashes = hashes
    val previousOccupied = occupied
    val previousHeads = heads
    val previousTails = tails
    hashes = new Array[Int](previousHashes.length << 1)
    occupied = new Array[Boolean](hashes.length)
    heads = Array.fill(hashes.length)(-1)
    tails = Array.fill(hashes.length)(-1)
    var previousSlot = 0
    while previousSlot < previousHashes.length do
      if previousOccupied(previousSlot) then
        val slot = findSlot(previousHashes(previousSlot))
        occupied(slot) = true
        hashes(slot) = previousHashes(previousSlot)
        heads(slot) = previousHeads(previousSlot)
        tails(slot) = previousTails(previousSlot)
      previousSlot += 1

  private def ensureNodeCapacity(required: Int): Unit =
    if required > links.length then
      var capacity = links.length
      while capacity < required do capacity = capacity << 1
      val expanded = Array.fill(capacity)(-1)
      Array.copy(links, 0, expanded, 0, links.length)
      links = expanded

  private def spread(hash: Int): Int = hash ^ (hash >>> 16)

/** Preparation-local mutable builder for an exact WL interner.
  *
  * Hashes select candidate buckets only. Every candidate is compared exactly before its colour is
  * reused. The completed builder freezes to the same immutable [[WLInterner]] retained by a trace.
  */
final private[grakern] class WLInternerBuilder[N, E] private (
    owner: ColourOwner,
    nodeHash: Hash[N],
    edgeHash: Hash[E],
    hashing: WLHashStrategy[N, E],
    codecs: WLCodecs[N, E],
    expectedSignatures: Int
):
  private val nodes =
    val values = mutable.ArrayBuffer.empty[WLSignature[N, E]]
    values.sizeHint(expectedSignatures)
    values
  private val buckets =
    new MutableWLInternerBuckets(expectedSignatures)
  private var matchedScratch = new Array[Boolean](16)

  def internInitial(nodeKey: N): WLColour =
    findInitial(nodeKey) match
      case Some(colour) => colour
      case None         =>
        val hash = hashing.initial(nodeKey)
        val colour =
          WLColour.create(owner, nodes.size, level = 0)
        append(
          hash,
          WLSignature.initial[N, E](
            colour,
            nodeKey,
            WLDescriptor.initial(nodeKey, codecs.node)
          )
        )
        colour

  def internRefined(
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Either[WLInternerError, WLColour] =
    internRefinedView(parent, WLIncidenceView.fromVector(neighbourhood))

  private[grakern] def internRefinedView(
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Either[WLInternerError, WLColour] =
    validateReferences(parent, neighbourhood) match
      case Left(error) => Left(error)
      case Right(_)    => Right(internRefinedViewTrusted(parent, neighbourhood))

  /** Allocation-lean refinement for a trace whose parent and neighbour colours were created by this
    * builder at the immediately preceding level. The engine establishes that invariant while
    * walking one packed corpus; external callers use the checked method above.
    */
  private[grakern] def internRefinedViewTrusted(
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): WLColour =
    val level = parent.level + 1
    val hash = hashing.refinedView(level, parent, neighbourhood)
    val existing = findRefinedIndex(hash, level, parent, neighbourhood)
    if existing >= 0 then nodes(existing).colour
    else
      val colour =
        WLColour.create(owner, nodes.size, level)
      val neighbours = Vector.newBuilder[WLNeighbour[E]]
      var neighbourIndex = 0
      while neighbourIndex < neighbourhood.size do
        neighbours += new WLNeighbour(
          neighbourhood.edgeKey(neighbourIndex),
          neighbourhood.orientation(neighbourIndex),
          neighbourhood.colour(neighbourIndex)
        )
        neighbourIndex += 1
      val parentDescriptor =
        signature(parent).flatMap(_.stableDescriptor)
      val descriptor =
        if parentDescriptor.isEmpty || codecs.edge.isEmpty then None
        else
          val described = Vector.newBuilder[
            (E, grakern.core.Orientation, Option[grakern.core.SemanticFingerprint])
          ]
          var descriptorIndex = 0
          while descriptorIndex < neighbourhood.size do
            described += (
              (
                neighbourhood.edgeKey(descriptorIndex),
                neighbourhood.orientation(descriptorIndex),
                signature(neighbourhood.colour(descriptorIndex)).flatMap(_.stableDescriptor)
              )
            )
            descriptorIndex += 1
          WLDescriptor.refined(
            level,
            parentDescriptor,
            described.result(),
            codecs.edge
          )
      append(
        hash,
        WLSignature.refined[N, E](
          colour,
          parent,
          neighbours.result(),
          descriptor
        )
      )
      colour

  def freeze(): WLInterner[N, E] =
    new WLInterner(
      owner,
      nodeHash,
      edgeHash,
      hashing,
      codecs,
      nodes.toVector,
      buckets.freeze()
    )

  private def signature(
      colour: WLColour
  ): Option[WLSignature[N, E]] =
    if owns(colour) &&
      colour.index >= 0 &&
      colour.index < nodes.size
    then
      val value = nodes(colour.index)
      Option.when(value.level == colour.level)(value)
    else None

  private def findInitial(
      nodeKey: N
  ): Option[WLColour] =
    val hash = hashing.initial(nodeKey)
    var candidate = buckets.head(hash)
    while candidate >= 0 do
      nodes(candidate) match
        case initial: WLSignature.Initial[N, E] if nodeHash.eqv(initial.nodeKey, nodeKey) =>
          return Some(initial.colour)
        case _ =>
      candidate = buckets.nextIndex(candidate)
    None

  private def findRefinedIndex(
      hash: Int,
      level: Int,
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Int =
    var candidate = buckets.head(hash)
    while candidate >= 0 do
      nodes(candidate) match
        case refined: WLSignature.Refined[N, E]
            if refined.level == level &&
              refined.previous.equals(parent) &&
              exactMultiset(refined.neighbourhood, neighbourhood) =>
          return candidate
        case _ =>
      candidate = buckets.nextIndex(candidate)
    -1

  private def exactMultiset(
      stored: Vector[WLNeighbour[E]],
      incoming: WLIncidenceView[E]
  ): Boolean =
    if stored.size != incoming.size then false
    else
      if incoming.size > matchedScratch.length then
        var capacity = matchedScratch.length << 1
        while capacity < incoming.size do capacity = capacity << 1
        matchedScratch = new Array[Boolean](capacity)
      else java.util.Arrays.fill(matchedScratch, 0, incoming.size, false)
      var storedIndex = 0
      while storedIndex < stored.size do
        val expected = stored(storedIndex)
        var candidate = 0
        var found = -1
        while candidate < incoming.size && found < 0 do
          if !matchedScratch(candidate) &&
            edgeHash.eqv(expected.edgeKey, incoming.edgeKey(candidate)) &&
            expected.orientation == incoming.orientation(candidate) &&
            expected.colour.equals(incoming.colour(candidate))
          then found = candidate
          candidate += 1
        if found < 0 then return false
        matchedScratch(found) = true
        storedIndex += 1
      true

  private def validateReferences(
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Either[WLInternerError, Unit] =
    if !owns(parent) then Left(WLInternerError.ForeignColour(parent))
    else
      var index = 0
      while index < neighbourhood.size do
        val colour = neighbourhood.colour(index)
        if !owns(colour) then
          return Left(
            WLInternerError.ForeignColour(colour)
          )
        if colour.level != parent.level then
          return Left(
            WLInternerError.InvalidNeighbourLevel(
              parent,
              colour
            )
          )
        index += 1
      Right(())

  private def append(
      hash: Int,
      signature: WLSignature[N, E]
  ): Unit =
    val index = nodes.size
    nodes += signature
    buckets.append(hash, index)
    ()

  private def owns(colour: WLColour): Boolean =
    colour.owner.eq(owner)

private[grakern] object WLInternerBuilder:
  def empty[N: Hash, E: Hash](
      codecs: WLCodecs[N, E],
      hashing: WLHashStrategy[N, E]
  ): WLInternerBuilder[N, E] =
    empty(codecs, hashing, expectedSignatures = 16)

  def empty[N: Hash, E: Hash](
      codecs: WLCodecs[N, E],
      hashing: WLHashStrategy[N, E],
      expectedSignatures: Int
  ): WLInternerBuilder[N, E] =
    new WLInternerBuilder(
      new ColourOwner,
      Hash[N],
      Hash[E],
      hashing,
      codecs,
      expectedSignatures
    )
