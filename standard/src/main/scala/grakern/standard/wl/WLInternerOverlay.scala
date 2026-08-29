package grakern.standard.wl

import scala.collection.mutable

/** An immutable query-local extension of a training interner.
  *
  * Known signatures reuse training colours. Novel signatures use a distinct owner shared only by
  * snapshots descended from this overlay.
  */
final class WLInternerOverlay[N, E] private[wl] (
    private[wl] val base: WLInterner[N, E],
    private[wl] val localOwner: ColourOwner,
    private[wl] val localNodes: Vector[WLSignature[N, E]],
    private[wl] val localBuckets: WLInternerBuckets
):
  def baseSignatureCount: Int = base.size
  def novelSignatureCount: Int = localNodes.size
  def signatureCount: Int = base.size + localNodes.size

  def isNovel(colour: WLColour): Boolean =
    ownsLocal(colour)

  def signature(colour: WLColour): Option[WLSignature[N, E]] =
    base.signature(colour).orElse {
      if ownsLocal(colour) then localNodes.lift(colour.index).filter(_.level == colour.level)
      else None
    }

  def internInitial(
      nodeKey: N
  ): (WLInternerOverlay[N, E], WLColour) =
    base.findInitial(nodeKey).orElse(findLocalInitial(nodeKey)) match
      case Some(colour) => (this, colour)
      case None         =>
        val hash = base.hashing.initial(nodeKey)
        val colour =
          WLColour.create(localOwner, localNodes.size, level = 0)
        val value =
          WLSignature.initial[N, E](
            colour,
            nodeKey,
            WLDescriptor.initial(nodeKey, base.codecs.node)
          )
        (appended(hash, value), colour)

  def internRefined(
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Either[WLInternerError, (WLInternerOverlay[N, E], WLColour)] =
    internRefinedView(parent, WLIncidenceView.fromVector(neighbourhood))

  private[grakern] def internRefinedView(
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Either[WLInternerError, (WLInternerOverlay[N, E], WLColour)] =
    validateReferences(parent, neighbourhood).map { _ =>
      val level = parent.level + 1
      var baseOwned = base.owns(parent)
      var ownershipIndex = 0
      while baseOwned && ownershipIndex < neighbourhood.size do
        baseOwned = base.owns(neighbourhood.colour(ownershipIndex))
        ownershipIndex += 1
      val known =
        if baseOwned
        then base.findRefinedView(level, parent, neighbourhood)
        else None

      known.orElse(
        findLocalRefined(level, parent, neighbourhood)
      ) match
        case Some(colour) => (this, colour)
        case None         =>
          val hash =
            base.hashing.refinedView(level, parent, neighbourhood)
          val colour =
            WLColour.create(localOwner, localNodes.size, level)
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
            if parentDescriptor.isEmpty || base.codecs.edge.isEmpty then None
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
                base.codecs.edge
              )
          val value =
            WLSignature.refined[N, E](
              colour,
              parent,
              neighbours.result(),
              descriptor
            )
          (appended(hash, value), colour)
    }

  def semanticallyEquivalent(
      left: WLColour,
      that: WLInternerOverlay[N, E],
      right: WLColour
  ): Boolean =
    (signature(left), that.signature(right)) match
      case (
            Some(leftInitial: WLSignature.Initial[N, E]),
            Some(rightInitial: WLSignature.Initial[N, E])
          ) =>
        base.nodeHash.eqv(
          leftInitial.nodeKey,
          rightInitial.nodeKey
        )
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
        crossTraceMultiset(
          leftRefined.neighbourhood,
          that,
          rightRefined.neighbourhood
        )
      case _ =>
        false

  private def findLocalInitial(nodeKey: N): Option[WLColour] =
    val hash = base.hashing.initial(nodeKey)
    val bucket = localBuckets.bucket(hash)
    if bucket < 0 then None
    else
      var position = localBuckets.start(bucket)
      val end = localBuckets.end(bucket)
      while position < end do
        localNodes(localBuckets.nodeIndex(position)) match
          case initial: WLSignature.Initial[N, E] if base.nodeHash.eqv(initial.nodeKey, nodeKey) =>
            return Some(initial.colour)
          case _ =>
        position += 1
      None

  private def findLocalRefined(
      level: Int,
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Option[WLColour] =
    val hash = base.hashing.refinedView(level, parent, neighbourhood)
    val bucket = localBuckets.bucket(hash)
    if bucket < 0 then None
    else
      var position = localBuckets.start(bucket)
      val end = localBuckets.end(bucket)
      while position < end do
        localNodes(localBuckets.nodeIndex(position)) match
          case refined: WLSignature.Refined[N, E]
              if refined.level == level &&
                refined.previous.equals(parent) &&
                WLInterner.exactMultisetView(
                  base.edgeHash,
                  refined.neighbourhood,
                  neighbourhood
                ) =>
            return Some(refined.colour)
          case _ =>
        position += 1
      None

  private def validateReferences(
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Either[WLInternerError, Unit] =
    if !owns(parent) then Left(WLInternerError.ForeignColour(parent))
    else
      var index = 0
      while index < neighbourhood.size do
        val colour = neighbourhood.colour(index)
        if !owns(colour) then return Left(WLInternerError.ForeignColour(colour))
        if colour.level != parent.level then
          return Left(WLInternerError.InvalidNeighbourLevel(parent, colour))
        index += 1
      Right(())

  private def owns(colour: WLColour): Boolean =
    base.owns(colour) || ownsLocal(colour)

  private def ownsLocal(colour: WLColour): Boolean =
    colour.owner.eq(localOwner)

  private def appended(
      hash: Int,
      signature: WLSignature[N, E]
  ): WLInternerOverlay[N, E] =
    new WLInternerOverlay(
      base,
      localOwner,
      localNodes :+ signature,
      localBuckets.appended(hash, localNodes.size)
    )

  private def crossTraceMultiset(
      left: Vector[WLNeighbour[E]],
      that: WLInternerOverlay[N, E],
      right: Vector[WLNeighbour[E]]
  ): Boolean =
    if left.size != right.size then false
    else
      val matched = Array.fill(right.size)(false)
      left.forall { expected =>
        val index =
          right.indices.find { candidate =>
            !matched(candidate) &&
            base.edgeHash.eqv(
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

object WLInternerOverlay:
  def apply[N, E](
      base: WLInterner[N, E]
  ): WLInternerOverlay[N, E] =
    new WLInternerOverlay(
      base,
      new ColourOwner,
      Vector.empty,
      WLInternerBuckets.empty
    )

/** Engine-local mutable construction path for an immutable query overlay. Public overlay updates
  * remain persistent; a packed query can build once and freeze once without allocating one map
  * snapshot per novel signature.
  */
final private[grakern] class WLInternerOverlayBuilder[N, E] private (
    base: WLInterner[N, E],
    localOwner: ColourOwner,
    expectedSignatures: Int
):
  private val localNodes =
    val values = mutable.ArrayBuffer.empty[WLSignature[N, E]]
    values.sizeHint(expectedSignatures)
    values
  private val localBuckets = new MutableWLInternerBuckets(expectedSignatures)

  def internInitial(nodeKey: N): WLColour =
    base.findInitial(nodeKey).orElse(findLocalInitial(nodeKey)) match
      case Some(colour) => colour
      case None         =>
        val hash = base.hashing.initial(nodeKey)
        val colour = WLColour.create(localOwner, localNodes.size, level = 0)
        append(
          hash,
          WLSignature.initial[N, E](
            colour,
            nodeKey,
            WLDescriptor.initial(nodeKey, base.codecs.node)
          )
        )
        colour

  def internRefinedView(
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Either[WLInternerError, WLColour] =
    validateReferences(parent, neighbourhood).map { _ =>
      val level = parent.level + 1
      var baseOwned = base.owns(parent)
      var ownershipIndex = 0
      while baseOwned && ownershipIndex < neighbourhood.size do
        baseOwned = base.owns(neighbourhood.colour(ownershipIndex))
        ownershipIndex += 1
      val known =
        if baseOwned then base.findRefinedView(level, parent, neighbourhood)
        else None
      known.orElse(findLocalRefined(level, parent, neighbourhood)) match
        case Some(colour) => colour
        case None         =>
          val hash = base.hashing.refinedView(level, parent, neighbourhood)
          val colour = WLColour.create(localOwner, localNodes.size, level)
          val neighbours = Vector.newBuilder[WLNeighbour[E]]
          var neighbourIndex = 0
          while neighbourIndex < neighbourhood.size do
            neighbours += new WLNeighbour(
              neighbourhood.edgeKey(neighbourIndex),
              neighbourhood.orientation(neighbourIndex),
              neighbourhood.colour(neighbourIndex)
            )
            neighbourIndex += 1
          val parentDescriptor = signature(parent).flatMap(_.stableDescriptor)
          val descriptor =
            if parentDescriptor.isEmpty || base.codecs.edge.isEmpty then None
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
                base.codecs.edge
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
    }

  def freeze(): WLInternerOverlay[N, E] =
    new WLInternerOverlay(
      base,
      localOwner,
      localNodes.toVector,
      localBuckets.freeze()
    )

  private def signature(colour: WLColour): Option[WLSignature[N, E]] =
    base.signature(colour).orElse {
      if ownsLocal(colour) && colour.index >= 0 && colour.index < localNodes.size then
        val value = localNodes(colour.index)
        Option.when(value.level == colour.level)(value)
      else None
    }

  private def findLocalInitial(nodeKey: N): Option[WLColour] =
    val hash = base.hashing.initial(nodeKey)
    var candidate = localBuckets.head(hash)
    while candidate >= 0 do
      localNodes(candidate) match
        case initial: WLSignature.Initial[N, E] if base.nodeHash.eqv(initial.nodeKey, nodeKey) =>
          return Some(initial.colour)
        case _ =>
      candidate = localBuckets.nextIndex(candidate)
    None

  private def findLocalRefined(
      level: Int,
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Option[WLColour] =
    val hash = base.hashing.refinedView(level, parent, neighbourhood)
    var candidate = localBuckets.head(hash)
    while candidate >= 0 do
      localNodes(candidate) match
        case refined: WLSignature.Refined[N, E]
            if refined.level == level &&
              refined.previous.equals(parent) &&
              WLInterner.exactMultisetView(
                base.edgeHash,
                refined.neighbourhood,
                neighbourhood
              ) =>
          return Some(refined.colour)
        case _ =>
      candidate = localBuckets.nextIndex(candidate)
    None

  private def validateReferences(
      parent: WLColour,
      neighbourhood: WLIncidenceView[E]
  ): Either[WLInternerError, Unit] =
    if !owns(parent) then Left(WLInternerError.ForeignColour(parent))
    else
      var index = 0
      while index < neighbourhood.size do
        val colour = neighbourhood.colour(index)
        if !owns(colour) then return Left(WLInternerError.ForeignColour(colour))
        if colour.level != parent.level then
          return Left(WLInternerError.InvalidNeighbourLevel(parent, colour))
        index += 1
      Right(())

  private def append(hash: Int, signature: WLSignature[N, E]): Unit =
    val index = localNodes.size
    localNodes += signature
    localBuckets.append(hash, index)

  private def owns(colour: WLColour): Boolean = base.owns(colour) || ownsLocal(colour)

  private def ownsLocal(colour: WLColour): Boolean = colour.owner.eq(localOwner)

private[grakern] object WLInternerOverlayBuilder:
  def apply[N, E](
      base: WLInterner[N, E],
      expectedSignatures: Int
  ): WLInternerOverlayBuilder[N, E] =
    new WLInternerOverlayBuilder(base, new ColourOwner, expectedSignatures)
