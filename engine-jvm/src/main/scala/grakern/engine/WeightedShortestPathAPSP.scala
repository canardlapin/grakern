package grakern.engine

import grakern.core.*
import grakern.standard.shortestpath.*
import java.util.{Arrays, Comparator, PriorityQueue}

enum WeightedShortestPathAPSPError[V]:
  case PackedCorpus(error: PackedCorpusError[V])
  case EdgeProjection(
      error: PrepareError[Nothing, EdgeRef[V]]
  )
  case DistanceCapacityExceeded(
      sample: SampleKey,
      vertexCount: Int,
      requestedEntries: Long
  )
  case DistanceOverflow(
      sample: SampleKey,
      source: V,
      edge: EdgeRef[V],
      prefix: Double,
      edgeLength: Double
  )

enum WeightedAPSPTraceError derives CanEqual:
  case EdgeLengthPlanMismatch(
      expected: Long,
      observed: Long
  )

final private[grakern] class WeightedAPSPTrace[V, N, E] private[engine] (
    val planNodeId: PlanNodeId,
    val edgeLengthPlanNodeId: PlanNodeId,
    private val corpus: PackedGraphCorpus[V, N, E],
    private val blocks: Vector[WeightedDistanceBlock],
    val stats: ShortestPathAPSPStats
):
  def sampleKeys: Vector[SampleKey] =
    corpus.sampleKeys

  def graphCount: Int =
    blocks.size

  def occurrences[Key](
      recipe: WeightedShortestPathRecipe[N, E, Key]
  ): Either[
    WeightedAPSPTraceError,
    Vector[
      Vector[ShortestPathOccurrence[V, Key, PathLength]]
    ]
  ] =
    if recipe.edgeLengths.planNodeId.number != edgeLengthPlanNodeId.number then
      Left(
        WeightedAPSPTraceError.EdgeLengthPlanMismatch(
          edgeLengthPlanNodeId.number,
          recipe.edgeLengths.planNodeId.number
        )
      )
    else
      val keys =
        corpus.categoricalNodeColumn(recipe.nodeKey.apply)
      Right(
        blocks.zipWithIndex.map { case (block, graphIndex) =>
          val range = corpus.graphVertexRange(graphIndex)
          val output = Vector.newBuilder[
            ShortestPathOccurrence[V, Key, PathLength]
          ]
          block.foreachReachablePair { (left, right, distance) =>
            val globalLeft = range.start + left
            val globalRight = range.start + right
            output += ShortestPathOccurrence(
              ShortestPathPart(
                UndirectedEndpoints(
                  keys(globalLeft),
                  keys(globalRight)
                ),
                distance
              ),
              ShortestPathWitness(
                corpus.sampleKeys(graphIndex),
                UndirectedEndpoints(
                  corpus.originalVertex(globalLeft),
                  corpus.originalVertex(globalRight)
                )
              )
            )
          }
          output.result()
        }
      )

final private class WeightedDistanceBlock(
    val vertexCount: Int,
    private val distances: Array[Double]
):
  def foreachReachablePair(
      consume: (Int, Int, PathLength) => Unit
  ): Unit =
    var left = 0
    while left < vertexCount do
      var right = left + 1
      while right < vertexCount do
        val distance = distances(left * vertexCount + right)
        if distance.isFinite then
          consume(
            left,
            right,
            PathLength.knownNonNegative(distance)
          )
        right += 1
      left += 1

private object WeightedDistanceBlock:
  private val Unreachable = Double.PositiveInfinity

  final private class QueueEntry(
      val vertex: Int,
      val distance: Double
  )

  private val QueueEntryOrder: Comparator[QueueEntry] =
    new Comparator[QueueEntry]:
      def compare(
          left: QueueEntry,
          right: QueueEntry
      ): Int =
        val byDistance =
          java.lang.Double.compare(
            left.distance,
            right.distance
          )
        if byDistance != 0 then byDistance
        else
          java.lang.Integer.compare(
            left.vertex,
            right.vertex
          )

  def fromGraph[V, N, E](
      corpus: PackedGraphCorpus[V, N, E],
      edgeLengths: NonNegativeEdgeColumn,
      graphIndex: Int
  ): Either[
    WeightedShortestPathAPSPError[V],
    (WeightedDistanceBlock, Long)
  ] =
    val range = corpus.graphVertexRange(graphIndex)
    val size = range.size
    val requestedEntries = size.toLong * size.toLong
    if requestedEntries > Int.MaxValue.toLong then
      Left(
        WeightedShortestPathAPSPError.DistanceCapacityExceeded(
          corpus.sampleKeys(graphIndex),
          size,
          requestedEntries
        )
      )
    else
      val matrix =
        Array.fill(requestedEntries.toInt)(Unreachable)
      val sourceDistances = new Array[Double](size)
      val queue =
        new PriorityQueue[QueueEntry](
          math.max(1, size),
          QueueEntryOrder
        )
      var failure: Option[
        WeightedShortestPathAPSPError[V]
      ] = None
      var source = 0
      while source < size && failure.isEmpty do
        Arrays.fill(sourceDistances, Unreachable)
        sourceDistances(source) = 0.0
        queue.clear()
        queue.add(new QueueEntry(source, 0.0)): Unit

        while !queue.isEmpty && failure.isEmpty do
          val current = queue.remove()
          if current.distance == sourceDistances(current.vertex) then
            val globalVertex = range.start + current.vertex
            var adjacencyIndex =
              corpus.adjacencyStart(globalVertex)
            val adjacencyEnd =
              corpus.adjacencyEnd(globalVertex)
            while adjacencyIndex < adjacencyEnd && failure.isEmpty do
              val globalNeighbor =
                corpus.adjacencyTarget(adjacencyIndex)
              val localNeighbor =
                globalNeighbor - range.start
              val edgeIndex =
                corpus.adjacencyEdge(adjacencyIndex)
              val edgeLength = edgeLengths(edgeIndex)
              val existing =
                sourceDistances(localNeighbor)
              val couldImprove =
                current.distance < existing &&
                  (
                    !existing.isFinite ||
                      edgeLength < existing - current.distance
                  )
              if couldImprove then
                val candidate =
                  current.distance + edgeLength
                if !candidate.isFinite then
                  failure = Some(
                    WeightedShortestPathAPSPError.DistanceOverflow(
                      corpus.sampleKeys(graphIndex),
                      corpus.originalVertex(
                        range.start + source
                      ),
                      corpus.packedEdges(edgeIndex).original,
                      current.distance,
                      edgeLength
                    )
                  )
                else
                  sourceDistances(localNeighbor) = candidate
                  queue.add(
                    new QueueEntry(
                      localNeighbor,
                      candidate
                    )
                  ): Unit
              adjacencyIndex += 1

        if failure.isEmpty then
          System.arraycopy(
            sourceDistances,
            0,
            matrix,
            source * size,
            size
          )
        source += 1

      failure match
        case Some(error) =>
          Left(error)
        case None =>
          var reachablePairs = 0L
          var left = 0
          while left < size do
            var right = left + 1
            while right < size do
              if matrix(left * size + right).isFinite then reachablePairs += 1L
              right += 1
            left += 1
          Right(
            new WeightedDistanceBlock(size, matrix) ->
              reachablePairs
          )

object WeightedShortestPathAPSP:
  private[grakern] def prepare[V, N, E](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      edgeLengths: WeightedEdgeLengths[E]
  ): Either[
    WeightedShortestPathAPSPError[V],
    WeightedAPSPTrace[V, N, E]
  ] =
    prepare(
      samples,
      edgeLengths,
      PlanNodeId.fresh()
    )

  private[engine] def prepare[V, N, E](
      samples: Vector[LabelledNeighbourhood[V, N, E]],
      edgeLengths: WeightedEdgeLengths[E],
      planNodeId: PlanNodeId
  ): Either[
    WeightedShortestPathAPSPError[V],
    WeightedAPSPTrace[V, N, E]
  ] =
    PackedGraphCorpus
      .pack(samples)
      .left
      .map(WeightedShortestPathAPSPError.PackedCorpus(_))
      .flatMap { corpus =>
        corpus
          .nonNegativeEdgeColumn(
            edgeLengths.descriptor
          )(edgeLengths.projection.apply)
          .left
          .map(
            WeightedShortestPathAPSPError.EdgeProjection(
              _
            )
          )
          .flatMap { edgeColumn =>
            val completed =
              Vector.newBuilder[WeightedDistanceBlock]
            var matrixEntries = 0L
            var reachablePairs = 0L
            var failure: Option[
              WeightedShortestPathAPSPError[V]
            ] = None
            var graphIndex = 0
            while graphIndex < corpus.graphCount && failure.isEmpty do
              WeightedDistanceBlock
                .fromGraph(
                  corpus,
                  edgeColumn,
                  graphIndex
                ) match
                case Right((block, graphReachablePairs)) =>
                  completed += block
                  matrixEntries +=
                    block.vertexCount.toLong *
                      block.vertexCount.toLong
                  reachablePairs += graphReachablePairs
                case Left(error) =>
                  failure = Some(error)
              graphIndex += 1
            failure.toLeft(
              new WeightedAPSPTrace(
                planNodeId,
                edgeLengths.planNodeId,
                corpus,
                completed.result(),
                ShortestPathAPSPStats(
                  graphTransforms = corpus.graphCount,
                  sourceSearches = corpus.vertexCount,
                  matrixEntries,
                  reachablePairs
                )
              )
            )
          }
      }
