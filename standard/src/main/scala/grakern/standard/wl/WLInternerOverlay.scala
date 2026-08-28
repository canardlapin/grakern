package grakern.standard.wl

/** An immutable query-local extension of a training interner.
  *
  * Known signatures reuse training colours. Novel signatures use a distinct owner shared only by
  * snapshots descended from this overlay.
  */
final class WLInternerOverlay[N, E] private[wl] (
    private[wl] val base: WLInterner[N, E],
    private[wl] val localOwner: ColourOwner,
    private[wl] val localNodes: Vector[WLSignature[N, E]],
    private[wl] val localBuckets: Map[Int, Vector[Int]]
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
    validateReferences(parent, neighbourhood).map { _ =>
      val level = parent.level + 1
      val known =
        if base.owns(parent) &&
          neighbourhood.forall(incidence => base.owns(incidence.colour))
        then base.findRefined(level, parent, neighbourhood)
        else None

      known.orElse(
        findLocalRefined(level, parent, neighbourhood)
      ) match
        case Some(colour) => (this, colour)
        case None         =>
          val hash =
            base.hashing.refined(level, parent, neighbourhood)
          val colour =
            WLColour.create(localOwner, localNodes.size, level)
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
              base.codecs.edge
            )
          val value =
            WLSignature.refined[N, E](
              colour,
              parent,
              neighbours,
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
    candidates(hash).collectFirst {
      case initial: WLSignature.Initial[N, E] if base.nodeHash.eqv(initial.nodeKey, nodeKey) =>
        initial.colour
    }

  private def findLocalRefined(
      level: Int,
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Option[WLColour] =
    val hash = base.hashing.refined(level, parent, neighbourhood)
    candidates(hash).collectFirst {
      case refined: WLSignature.Refined[N, E]
          if refined.level == level &&
            refined.previous.equals(parent) &&
            WLInterner.exactMultiset(base.edgeHash, refined.neighbourhood, neighbourhood) =>
        refined.colour
    }

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

  private def owns(colour: WLColour): Boolean =
    base.owns(colour) || ownsLocal(colour)

  private def ownsLocal(colour: WLColour): Boolean =
    colour.owner.eq(localOwner)

  private def candidates(hash: Int): Vector[WLSignature[N, E]] =
    localBuckets
      .getOrElse(hash, Vector.empty)
      .flatMap(localNodes.lift)

  private def appended(
      hash: Int,
      signature: WLSignature[N, E]
  ): WLInternerOverlay[N, E] =
    val bucket =
      localBuckets.getOrElse(hash, Vector.empty) :+ localNodes.size
    new WLInternerOverlay(
      base,
      localOwner,
      localNodes :+ signature,
      localBuckets.updated(hash, bucket)
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
      Map.empty
    )
