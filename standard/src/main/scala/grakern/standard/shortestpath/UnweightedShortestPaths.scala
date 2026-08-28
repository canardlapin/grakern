package grakern.standard.shortestpath

import grakern.core.*

/** The semantic unweighted shortest-path decomposition.
  *
  * Each reachable unordered pair of distinct vertices is emitted exactly once. This reference
  * realization intentionally uses Floyd-Warshall so optimized BFS lowerings can be checked against
  * an independent algorithm. Every incidence is traversable in both directions (undirected
  * semantics); directed shortest paths are a separate recipe.
  */
final class UnweightedShortestPathRecipe[N, E, Key] private[shortestpath] (
    val planNodeId: PlanNodeId,
    private[grakern] val nodeKey: Projection[N, Key]
):
  def fingerprintability: Fingerprintability =
    Fingerprintability.combine(
      schema = "grakern.shortest-path.unweighted.v1",
      atoms = Vector.empty,
      children = Vector(nodeKey.fingerprintAt(planNodeId))
    )

  def materializeReference[V](
      sample: LabelledNeighbourhood[V, N, E]
  ): Either[
    DecompositionError,
    Vector[ShortestPathOccurrence[V, Key, HopDistance]]
  ] =
    UnweightedShortestPathReference.materialize(sample, nodeKey)

  def decomposition[V]: Decomposition[
    LabelledNeighbourhood[V, N, E],
    ShortestPathOccurrence[V, Key, HopDistance]
  ] =
    new Decomposition[
      LabelledNeighbourhood[V, N, E],
      ShortestPathOccurrence[V, Key, HopDistance]
    ]:
      def foreachWeighted(
          sample: LabelledNeighbourhood[V, N, E]
      )(
          emit: (
              ShortestPathOccurrence[V, Key, HopDistance],
              NonNegativeFinite
          ) => Either[DecompositionError, Unit]
      ): Either[DecompositionError, Unit] =
        materializeReference(sample).flatMap { occurrences =>
          val iterator = occurrences.iterator
          var failure: Option[DecompositionError] = None
          while iterator.hasNext && failure.isEmpty do
            emit(iterator.next(), NonNegativeFinite.one) match
              case Left(error) => failure = Some(error)
              case Right(_)    => ()
          failure.toLeft(())
        }

object UnweightedShortestPaths:
  def parts[N, E, Key](
      nodeKey: Projection[N, Key]
  ): UnweightedShortestPathRecipe[N, E, Key] =
    new UnweightedShortestPathRecipe(
      PlanNodeId.fresh(),
      nodeKey
    )

private object UnweightedShortestPathReference:
  def materialize[V, N, E, Key](
      sample: LabelledNeighbourhood[V, N, E],
      nodeKey: Projection[N, Key]
  ): Either[
    DecompositionError,
    Vector[ShortestPathOccurrence[V, Key, HopDistance]]
  ] =
    val vertices = sample.vertices.iterator.toVector
    val keys = vertices.map(vertex => nodeKey(sample.nodeValue(vertex)))
    val distances = allPairsDistances(sample, vertices)
    val occurrences = Vector.newBuilder[
      ShortestPathOccurrence[V, Key, HopDistance]
    ]
    var left = 0
    while left < vertices.size do
      var right = left + 1
      while right < vertices.size do
        distances(left)(right).foreach { distance =>
          occurrences += ShortestPathOccurrence(
            ShortestPathPart(
              UndirectedEndpoints(keys(left), keys(right)),
              HopDistance.knownPositive(distance.toInt)
            ),
            ShortestPathWitness(
              sample.key,
              UndirectedEndpoints(vertices(left), vertices(right))
            )
          )
        }
        right += 1
      left += 1
    Right(occurrences.result())

  private def allPairsDistances[V, N, E](
      sample: LabelledNeighbourhood[V, N, E],
      vertices: Vector[V]
  ): Array[Array[Option[Long]]] =
    given cats.kernel.Hash[V] = sample.vertexHash
    val size = vertices.size
    val index = vertices.iterator.zipWithIndex.map { case (v, i) => HashKey(v) -> i }.toMap
    val distances =
      Array.fill[Option[Long]](size, size)(None)
    var row = 0
    while row < size do
      distances(row)(row) = Some(0L)
      sample.incidences(vertices(row)).foreach { incidence =>
        index.get(HashKey(incidence.other)).foreach { column =>
          if column != row then
            distances(row)(column) = Some(1L)
            distances(column)(row) = Some(1L)
        }
      }
      row += 1

    var intermediate = 0
    while intermediate < size do
      var left = 0
      while left < size do
        var right = 0
        while right < size do
          val candidate =
            for
              toIntermediate <- distances(left)(intermediate)
              fromIntermediate <- distances(intermediate)(right)
            yield toIntermediate + fromIntermediate
          candidate.foreach { value =>
            distances(left)(right) match
              case Some(current) if current <= value => ()
              case _                                 => distances(left)(right) = Some(value)
          }
          right += 1
        left += 1
      intermediate += 1
    distances
