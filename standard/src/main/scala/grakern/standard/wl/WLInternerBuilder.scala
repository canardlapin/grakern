package grakern.standard.wl

import cats.kernel.Hash
import scala.collection.mutable

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
    codecs: WLCodecs[N, E]
):
  private val nodes =
    mutable.ArrayBuffer.empty[WLSignature[N, E]]
  private val buckets =
    mutable.HashMap.empty[
      Int,
      mutable.ArrayBuffer[Int]
    ]

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
    validateReferences(parent, neighbourhood).map { _ =>
      val level = parent.level + 1
      findRefined(level, parent, neighbourhood) match
        case Some(colour) => colour
        case None         =>
          val hash =
            hashing.refined(level, parent, neighbourhood)
          val colour =
            WLColour.create(owner, nodes.size, level)
          val neighbours =
            neighbourhood.map { incidence =>
              new WLNeighbour(incidence.edgeKey, incidence.orientation, incidence.colour)
            }
          val descriptor =
            WLDescriptor.refined(
              level,
              signature(parent).flatMap(_.stableDescriptor),
              neighbourhood.map { incidence =>
                (
                  incidence.edgeKey,
                  incidence.orientation,
                  signature(incidence.colour).flatMap(_.stableDescriptor)
                )
              },
              codecs.edge
            )
          append(
            hash,
            WLSignature.refined[N, E](
              colour,
              parent,
              neighbours,
              descriptor
            )
          )
          colour
    }

  def freeze(): WLInterner[N, E] =
    new WLInterner(
      owner,
      nodeHash,
      edgeHash,
      hashing,
      codecs,
      nodes.toVector,
      buckets.iterator.map { case (hash, indices) =>
        hash -> indices.toVector
      }.toMap
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
    val candidates =
      buckets.getOrElse(hash, WLInternerBuilder.emptyBucket)
    var index = 0
    while index < candidates.size do
      nodes(candidates(index)) match
        case initial: WLSignature.Initial[N, E] if nodeHash.eqv(initial.nodeKey, nodeKey) =>
          return Some(initial.colour)
        case _ =>
      index += 1
    None

  private def findRefined(
      level: Int,
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Option[WLColour] =
    val hash =
      hashing.refined(level, parent, neighbourhood)
    val candidates =
      buckets.getOrElse(hash, WLInternerBuilder.emptyBucket)
    var index = 0
    while index < candidates.size do
      nodes(candidates(index)) match
        case refined: WLSignature.Refined[N, E]
            if refined.level == level &&
              refined.previous.equals(parent) &&
              WLInterner.exactMultiset(
                edgeHash,
                refined.neighbourhood,
                neighbourhood
              ) =>
          return Some(refined.colour)
        case _ =>
      index += 1
    None

  private def validateReferences(
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Either[WLInternerError, Unit] =
    if !owns(parent) then Left(WLInternerError.ForeignColour(parent))
    else
      var index = 0
      while index < neighbourhood.size do
        val colour = neighbourhood(index).colour
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
    buckets
      .getOrElseUpdate(
        hash,
        mutable.ArrayBuffer.empty[Int]
      )
      .addOne(index)
    ()

  private def owns(colour: WLColour): Boolean =
    colour.owner.eq(owner)

private[grakern] object WLInternerBuilder:
  private val emptyBucket =
    mutable.ArrayBuffer.empty[Int]

  def empty[N: Hash, E: Hash](
      codecs: WLCodecs[N, E],
      hashing: WLHashStrategy[N, E]
  ): WLInternerBuilder[N, E] =
    new WLInternerBuilder(
      new ColourOwner,
      Hash[N],
      Hash[E],
      hashing,
      codecs
    )
