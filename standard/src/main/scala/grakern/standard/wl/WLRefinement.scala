package grakern.standard.wl

import cats.kernel.Hash
import grakern.core.*

enum WLRefinementError:
  case NegativeRounds(value: Int)

/** The immutable semantic inputs to one WL refinement computation.
  *
  * Reusing this value reuses its session-local plan identity. Execution state and corpus-specific
  * colours live in a prepared trace.
  */
final class WLRefinement[N, E, NodeKey, EdgeKey] private (
    val rounds: Int,
    val planNodeId: PlanNodeId,
    private[grakern] val nodeKey: Projection[N, NodeKey],
    private[grakern] val edgeKey: Projection[E, EdgeKey],
    private[grakern] val codecs: WLCodecs[NodeKey, EdgeKey],
    private[grakern] val hashing: WLHashStrategy[NodeKey, EdgeKey]
) extends Refinement:
  def fingerprintability: Fingerprintability =
    Fingerprintability.combine(
      // v2: neighbourhoods carry orientation relative to the refined vertex (ADR-009).
      schema = "grakern.wl-refinement.v2",
      atoms = Vector(rounds.toString),
      children = Vector(
        nodeKey.fingerprintAt(planNodeId),
        edgeKey.fingerprintAt(planNodeId),
        codecs.fingerprintability(planNodeId)
      )
    )

object WLRefinement:
  def create[
      N,
      E,
      NodeKey: Hash,
      EdgeKey: Hash
  ](
      rounds: Int,
      nodeKey: Projection[N, NodeKey],
      edgeKey: Projection[E, EdgeKey],
      codecs: WLCodecs[NodeKey, EdgeKey]
  ): Either[
    WLRefinementError,
    WLRefinement[N, E, NodeKey, EdgeKey]
  ] =
    create(
      rounds,
      nodeKey,
      edgeKey,
      codecs,
      WLHashStrategy.default[NodeKey, EdgeKey]
    )

  private[grakern] def create[
      N,
      E,
      NodeKey,
      EdgeKey
  ](
      rounds: Int,
      nodeKey: Projection[N, NodeKey],
      edgeKey: Projection[E, EdgeKey],
      codecs: WLCodecs[NodeKey, EdgeKey],
      hashing: WLHashStrategy[NodeKey, EdgeKey]
  ): Either[
    WLRefinementError,
    WLRefinement[N, E, NodeKey, EdgeKey]
  ] =
    if rounds < 0 then Left(WLRefinementError.NegativeRounds(rounds))
    else
      Right(
        new WLRefinement(
          rounds,
          PlanNodeId.fresh(),
          nodeKey,
          edgeKey,
          codecs,
          hashing
        )
      )
