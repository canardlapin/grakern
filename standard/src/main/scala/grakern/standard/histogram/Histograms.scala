package grakern.standard.histogram

import grakern.core.*

enum HistogramFeatureKind derives CanEqual:
  case Vertex
  case Edge

final case class HistogramFeature[+Key](
    kind: HistogramFeatureKind,
    key: Key
)

enum HistogramWitness[V]:
  case Vertex(sample: SampleKey, vertex: V)
  case Edge(sample: SampleKey, edge: EdgeRef[V])

final case class HistogramOccurrence[V, +Key](
    feature: HistogramFeature[Key],
    witness: HistogramWitness[V]
)

final class HistogramRecipe[N, E, Key] private[histogram] (
    val planNodeId: PlanNodeId,
    private[grakern] val emitter: HistogramEmitter[N, E, Key]
):
  def fingerprintability: Fingerprintability =
    emitter.fingerprintability(planNodeId)

  def decomposition[V]: Decomposition[
    LabelledNeighbourhood[V, N, E],
    HistogramOccurrence[V, Key]
  ] =
    emitter.decomposition

sealed private[grakern] trait HistogramEmitter[N, E, Key]:
  def decomposition[V]: Decomposition[
    LabelledNeighbourhood[V, N, E],
    HistogramOccurrence[V, Key]
  ]

  def fingerprintability(
      node: PlanNodeId
  ): Fingerprintability

private[histogram] object HistogramEmitter:
  final class Vertices[N, E, Key](
      key: Projection[N, Key]
  ) extends HistogramEmitter[N, E, Key]:
    def decomposition[V]: Decomposition[
      LabelledNeighbourhood[V, N, E],
      HistogramOccurrence[V, Key]
    ] =
      new Decomposition[
        LabelledNeighbourhood[V, N, E],
        HistogramOccurrence[V, Key]
      ]:
        def foreachWeighted(
            sample: LabelledNeighbourhood[V, N, E]
        )(
            emit: (
                HistogramOccurrence[V, Key],
                NonNegativeFinite
            ) => Either[DecompositionError, Unit]
        ): Either[DecompositionError, Unit] =
          val vertices = sample.vertices.iterator
          var failure: Option[DecompositionError] = None
          while vertices.hasNext && failure.isEmpty do
            val vertex = vertices.next()
            emit(
              HistogramOccurrence(
                HistogramFeature(
                  HistogramFeatureKind.Vertex,
                  key(sample.nodeValue(vertex))
                ),
                HistogramWitness.Vertex(
                  sample.key,
                  vertex
                )
              ),
              NonNegativeFinite.one
            ) match
              case Left(error) => failure = Some(error)
              case Right(_)    => ()
          failure.toLeft(())

    def fingerprintability(
        node: PlanNodeId
    ): Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.vertex-histogram.v1",
        atoms = Vector.empty,
        children = Vector(key.fingerprintAt(node))
      )

  final class Edges[N, E, Key](
      key: Projection[E, Key]
  ) extends HistogramEmitter[N, E, Key]:
    def decomposition[V]: Decomposition[
      LabelledNeighbourhood[V, N, E],
      HistogramOccurrence[V, Key]
    ] =
      new Decomposition[
        LabelledNeighbourhood[V, N, E],
        HistogramOccurrence[V, Key]
      ]:
        def foreachWeighted(
            sample: LabelledNeighbourhood[V, N, E]
        )(
            emit: (
                HistogramOccurrence[V, Key],
                NonNegativeFinite
            ) => Either[DecompositionError, Unit]
        ): Either[DecompositionError, Unit] =
          val edges = Neighbourhoods.canonicalEdges(sample).iterator
          var failure: Option[DecompositionError] = None
          while edges.hasNext && failure.isEmpty do
            val edge = edges.next()
            emit(
              HistogramOccurrence(
                HistogramFeature(
                  HistogramFeatureKind.Edge,
                  key(edge.value)
                ),
                HistogramWitness.Edge(sample.key, edge.ref)
              ),
              NonNegativeFinite.one
            ) match
              case Left(error) => failure = Some(error)
              case Right(_)    => ()
          failure.toLeft(())

    def fingerprintability(
        node: PlanNodeId
    ): Fingerprintability =
      Fingerprintability.combine(
        schema = "grakern.edge-histogram.v1",
        atoms = Vector.empty,
        children = Vector(key.fingerprintAt(node))
      )

object Histograms:
  def vertices[N, E, Key](
      key: Projection[N, Key]
  ): HistogramRecipe[N, E, Key] =
    new HistogramRecipe(
      PlanNodeId.fresh(),
      new HistogramEmitter.Vertices(key)
    )

  def edges[N, E, Key](
      key: Projection[E, Key]
  ): HistogramRecipe[N, E, Key] =
    new HistogramRecipe(
      PlanNodeId.fresh(),
      new HistogramEmitter.Edges(key)
    )
