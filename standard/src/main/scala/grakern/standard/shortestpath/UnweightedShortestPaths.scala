package grakern.standard.shortestpath

import grakern.core.*
import grakern.graph4s.GraphSample

/** The semantic unweighted shortest-path decomposition.
  *
  * Each reachable unordered pair of distinct vertices is emitted exactly once. This reference
  * realization intentionally uses Floyd-Warshall so optimized BFS lowerings can be checked against
  * an independent algorithm.
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
      sample: GraphSample[V, N, E]
  ): Either[
    DecompositionError,
    Vector[ShortestPathOccurrence[V, Key, HopDistance]]
  ] =
    UnweightedShortestPathReference.materialize(sample, nodeKey)

  def decomposition[V]: Decomposition[
    GraphSample[V, N, E],
    ShortestPathOccurrence[V, Key, HopDistance]
  ] =
    new Decomposition[
      GraphSample[V, N, E],
      ShortestPathOccurrence[V, Key, HopDistance]
    ]:
      def foreachWeighted(
          sample: GraphSample[V, N, E]
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
      sample: GraphSample[V, N, E],
      nodeKey: Projection[N, Key]
  ): Either[
    DecompositionError,
    Vector[ShortestPathOccurrence[V, Key, HopDistance]]
  ] =
    val vertices = sample.topology.vertices.iterator.toVector
    projectNodeKeys(sample, vertices, nodeKey).map { keys =>
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
      occurrences.result()
    }

  private def projectNodeKeys[V, N, E, Key](
      sample: GraphSample[V, N, E],
      vertices: Vector[V],
      nodeKey: Projection[N, Key]
  ): Either[DecompositionError, Vector[Key]] =
    val keys = Vector.newBuilder[Key]
    val iterator = vertices.iterator
    var failure: Option[DecompositionError] = None
    while iterator.hasNext && failure.isEmpty do
      sample.nodeValue(iterator.next()) match
        case Some(value) =>
          keys += nodeKey(value)
        case None =>
          failure = Some(
            DecompositionError.InvariantViolation(
              sample.key,
              DecompositionInvariant.TotalNodeField
            )
          )
    failure.toLeft(keys.result())

  private def allPairsDistances[V, N, E](
      sample: GraphSample[V, N, E],
      vertices: Vector[V]
  ): Array[Array[Option[Long]]] =
    val size = vertices.size
    val distances =
      Array.fill[Option[Long]](size, size)(None)
    var row = 0
    while row < size do
      distances(row)(row) = Some(0L)
      var column = row + 1
      while column < size do
        if sample.topology.containsEdge(vertices(row), vertices(column)) then
          distances(row)(column) = Some(1L)
          distances(column)(row) = Some(1L)
        column += 1
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
