package grakern.standard.wl

import grakern.core.*

final private[wl] class ColourOwner

/** A compact colour identifier whose equality includes its owning trace.
  *
  * The integer index is an execution representation. Equal indices from different traces are
  * different colours.
  */
final class WLColour private[wl] (
    private[wl] val owner: ColourOwner,
    val index: Int,
    val level: Int
):
  def sameTrace(that: WLColour): Boolean =
    owner eq that.owner

  override def equals(other: Any): Boolean =
    other match
      case that: WLColour =>
        (owner eq that.owner) &&
        index == that.index &&
        level == that.level
      case _ =>
        false

  override def hashCode(): Int =
    var result = owner.hashCode()
    result = 31 * result + index
    31 * result + level

  override def toString: String =
    s"WLColour(level=$level,index=$index)"

private[wl] object WLColour:
  def create(owner: ColourOwner, index: Int, level: Int): WLColour =
    new WLColour(owner, index, level)

/** One element of a refinement neighbourhood: the edge key, the orientation of that edge relative
  * to the vertex being refined, and the neighbour's previous colour.
  */
final case class WLIncidence[+E](
    edgeKey: E,
    orientation: Orientation,
    colour: WLColour
)

object WLIncidence:
  def undirected[E](edgeKey: E, colour: WLColour): WLIncidence[E] =
    WLIncidence(edgeKey, Orientation.Undirected, colour)

final class WLNeighbour[E] private[wl] (
    val edgeKey: E,
    val orientation: Orientation,
    val colour: WLColour
)

sealed trait WLSignature[N, E]:
  def colour: WLColour
  def level: Int = colour.level
  def parent: Option[WLColour]
  def stableDescriptor: Option[SemanticFingerprint]

object WLSignature:
  final class Initial[N, E] private[wl] (
      val colour: WLColour,
      val nodeKey: N,
      val stableDescriptor: Option[SemanticFingerprint]
  ) extends WLSignature[N, E]:
    val parent: Option[WLColour] = None

  final class Refined[N, E] private[wl] (
      val colour: WLColour,
      val previous: WLColour,
      val neighbourhood: Vector[WLNeighbour[E]],
      val stableDescriptor: Option[SemanticFingerprint]
  ) extends WLSignature[N, E]:
    val parent: Option[WLColour] = Some(previous)

  private[wl] def initial[N, E](
      colour: WLColour,
      nodeKey: N,
      descriptor: Option[SemanticFingerprint]
  ): WLSignature[N, E] =
    new Initial(colour, nodeKey, descriptor)

  private[wl] def refined[N, E](
      colour: WLColour,
      previous: WLColour,
      neighbourhood: Vector[WLNeighbour[E]],
      descriptor: Option[SemanticFingerprint]
  ): WLSignature[N, E] =
    new Refined(colour, previous, neighbourhood, descriptor)

final case class WLSource[+V](
    sample: SampleKey,
    vertex: V
)

final case class WLSignatureOccurrence[+V](
    source: WLSource[V],
    level: Int,
    colour: WLColour
)

final case class WLCodecs[N, E](
    node: Option[StableCodec[N]],
    edge: Option[StableCodec[E]]
):
  def fingerprintability(nodeId: PlanNodeId): Fingerprintability =
    val blockers =
      Vector(
        node.fold(
          Some(FingerprintBlocker.MissingCodec(nodeId, "WL node key"))
        )(_ => None),
        edge.fold(
          Some(FingerprintBlocker.MissingCodec(nodeId, "WL edge key"))
        )(_ => None)
      ).flatten

    FingerprintBlockers.fromVector(blockers) match
      case Some(problems) =>
        Fingerprintability.Ephemeral(problems)
      case None =>
        val nodeCodec = node.get
        val edgeCodec = edge.get
        Fingerprintability.Durable(
          SemanticFingerprint.compose(
            schema = "grakern.wl-codecs.v2",
            atoms = Vector(
              nodeCodec.id.text,
              nodeCodec.version.number.toString,
              edgeCodec.id.text,
              edgeCodec.version.number.toString
            ),
            children = Vector.empty
          )
        )

private[wl] object WLDescriptor:
  /** Stable spelling of an orientation inside a durable descriptor. Version 2 of the neighbour
    * schema introduced this atom; undirected-era descriptors (v1) are deliberately incompatible so
    * cached artifacts are never reused across the change.
    */
  private def orientationAtom(orientation: Orientation): String =
    orientation match
      case Orientation.Outgoing   => "out"
      case Orientation.Incoming   => "in"
      case Orientation.Undirected => "undirected"

  def initial[N](
      nodeKey: N,
      codec: Option[StableCodec[N]]
  ): Option[SemanticFingerprint] =
    codec.map { stable =>
      SemanticFingerprint.compose(
        schema = "grakern.wl-signature.initial.v1",
        atoms = Vector(
          stable.id.text,
          stable.version.number.toString,
          stable.encode(nodeKey)
        ),
        children = Vector.empty
      )
    }

  def refined[E](
      level: Int,
      parent: Option[SemanticFingerprint],
      neighbours: Vector[(E, Orientation, Option[SemanticFingerprint])],
      codec: Option[StableCodec[E]]
  ): Option[SemanticFingerprint] =
    for
      parentDescriptor <- parent
      edgeCodec <- codec
      neighbourDescriptors <- sequence(
        neighbours.map { case (edgeKey, orientation, signature) =>
          signature.map { child =>
            SemanticFingerprint.compose(
              schema = "grakern.wl-signature.neighbour.v2",
              atoms = Vector(
                edgeCodec.id.text,
                edgeCodec.version.number.toString,
                edgeCodec.encode(edgeKey),
                orientationAtom(orientation)
              ),
              children = Vector(child)
            )
          }
        }
      )
    yield SemanticFingerprint.compose(
      schema = "grakern.wl-signature.refined.v2",
      atoms = Vector(level.toString),
      children = parentDescriptor +:
        neighbourDescriptors.sortBy(_.value)
    )

  private def sequence[A](values: Vector[Option[A]]): Option[Vector[A]] =
    values.foldLeft(Option(Vector.empty[A])) { (current, value) =>
      for
        accumulated <- current
        next <- value
      yield accumulated :+ next
    }
