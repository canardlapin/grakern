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
        val unordered =
          neighbourhood.foldLeft(0) { (current, incidence) =>
            current +
              MurmurHash3.mix(
                Hash[E].hash(incidence.edgeKey) ^ orientationSalt(incidence.orientation),
                incidence.colour.index
              )
          }
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

enum WLInternerError:
  case ForeignColour(colour: WLColour)
  case InvalidNeighbourLevel(
      parent: WLColour,
      neighbour: WLColour
  )

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
    private[wl] val buckets: Map[Int, Vector[Int]]
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
    candidates(hash).collectFirst {
      case initial: WLSignature.Initial[N, E] if nodeHash.eqv(initial.nodeKey, nodeKey) =>
        initial.colour
    }

  private[wl] def findRefined(
      level: Int,
      parent: WLColour,
      neighbourhood: Vector[WLIncidence[E]]
  ): Option[WLColour] =
    val hash = hashing.refined(level, parent, neighbourhood)
    candidates(hash).collectFirst {
      case refined: WLSignature.Refined[N, E]
          if refined.level == level &&
            refined.previous.equals(parent) &&
            WLInterner.exactMultiset(edgeHash, refined.neighbourhood, neighbourhood) =>
        refined.colour
    }

  private[wl] def owns(colour: WLColour): Boolean =
    colour.owner.eq(owner)

  private[wl] def candidates(
      hash: Int
  ): Vector[WLSignature[N, E]] =
    buckets
      .getOrElse(hash, Vector.empty)
      .flatMap(nodes.lift)

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
    val updatedBucket =
      buckets.getOrElse(hash, Vector.empty) :+ nodes.size
    new WLInterner(
      owner,
      nodeHash,
      edgeHash,
      hashing,
      codecs,
      nodes :+ signature,
      buckets.updated(hash, updatedBucket)
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
      Map.empty
    )

  /** Exact multiset equality of a stored neighbourhood against an incoming one: edge key under
    * `Hash[E]`, orientation, and colour must all match with multiplicity.
    */
  private[wl] def exactMultiset[E](
      edgeHash: Hash[E],
      stored: Vector[WLNeighbour[E]],
      incoming: Vector[WLIncidence[E]]
  ): Boolean =
    if stored.size != incoming.size then false
    else
      val matched = Array.fill(incoming.size)(false)
      stored.forall { expected =>
        val index =
          incoming.indices.find { candidate =>
            !matched(candidate) &&
            edgeHash.eqv(
              expected.edgeKey,
              incoming(candidate).edgeKey
            ) &&
            expected.orientation == incoming(candidate).orientation &&
            expected.colour.equals(incoming(candidate).colour)
          }
        index.foreach(candidate => matched(candidate) = true)
        index.nonEmpty
      }
