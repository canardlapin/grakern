package grakern.engine

import grakern.core.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import java.util.Arrays

final case class ShortestPathAPSPStats(
    graphTransforms: Int,
    sourceSearches: Int,
    matrixEntries: Long,
    reachableUnorderedPairs: Long
)

enum ShortestPathAPSPError[V]:
  case PackedCorpus(error: PackedCorpusError[V])
  case DistanceCapacityExceeded(
      sample: SampleKey,
      vertexCount: Int,
      requestedEntries: Long
  )

/** One packed, exact unweighted APSP transform.
  *
  * The distance blocks are independent of endpoint projections. Multiple shortest-path recipes may
  * therefore consume this value without rerunning BFS.
  */
final private[grakern] class UnweightedAPSPTrace[V, N, E] private[engine] (
    val planNodeId: PlanNodeId,
    private val corpus: PackedGraphCorpus[V, N, E],
    private val blocks: Vector[UnweightedDistanceBlock],
    val stats: ShortestPathAPSPStats
):
  def sampleKeys: Vector[SampleKey] =
    corpus.sampleKeys

  def graphCount: Int =
    blocks.size

  def occurrences[Key](
      recipe: UnweightedShortestPathRecipe[N, E, Key]
  ): Vector[
    Vector[ShortestPathOccurrence[V, Key, HopDistance]]
  ] =
    val keys = corpus.categoricalNodeColumn(recipe.nodeKey.apply)
    blocks.zipWithIndex.map { case (block, graphIndex) =>
      val range = corpus.graphVertexRange(graphIndex)
      val output = Vector.newBuilder[
        ShortestPathOccurrence[V, Key, HopDistance]
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

final private class UnweightedDistanceBlock(
    val vertexCount: Int,
    private val distances: Array[Int]
):
  def foreachReachablePair(
      consume: (Int, Int, HopDistance) => Unit
  ): Unit =
    var left = 0
    while left < vertexCount do
      var right = left + 1
      while right < vertexCount do
        val distance = distances(left * vertexCount + right)
        if distance > 0 then
          consume(
            left,
            right,
            HopDistance.knownPositive(distance)
          )
        right += 1
      left += 1

private object UnweightedDistanceBlock:
  val Unreachable = -1

  def fromGraph[V, N, E](
      corpus: PackedGraphCorpus[V, N, E],
      graphIndex: Int
  ): Either[
    ShortestPathAPSPError[V],
    (UnweightedDistanceBlock, Long)
  ] =
    val range = corpus.graphVertexRange(graphIndex)
    val size = range.size
    val requestedEntries = size.toLong * size.toLong
    if requestedEntries > Int.MaxValue.toLong then
      Left(
        ShortestPathAPSPError.DistanceCapacityExceeded(
          corpus.sampleKeys(graphIndex),
          size,
          requestedEntries
        )
      )
    else
      val matrix =
        Array.fill(requestedEntries.toInt)(Unreachable)
      val queue = new Array[Int](size)
      val sourceDistances = new Array[Int](size)
      var reachablePairs = 0L
      var source = 0
      while source < size do
        Arrays.fill(sourceDistances, Unreachable)
        sourceDistances(source) = 0
        var head = 0
        var tail = 1
        queue(0) = source

        while head < tail do
          val localVertex = queue(head)
          head += 1
          val globalVertex = range.start + localVertex
          val neighbors = corpus.neighbors(globalVertex)
          var neighborIndex = 0
          while neighborIndex < neighbors.size do
            val localNeighbor = neighbors(neighborIndex) - range.start
            if sourceDistances(localNeighbor) == Unreachable then
              sourceDistances(localNeighbor) = sourceDistances(localVertex) + 1
              queue(tail) = localNeighbor
              tail += 1
            neighborIndex += 1

        System.arraycopy(
          sourceDistances,
          0,
          matrix,
          source * size,
          size
        )
        var target = source + 1
        while target < size do
          if sourceDistances(target) > 0 then reachablePairs += 1L
          target += 1
        source += 1
      Right(new UnweightedDistanceBlock(size, matrix) -> reachablePairs)

object ShortestPathAPSP:
  private[grakern] def prepare[V, N, E](
      samples: Vector[GraphSample[V, N, E]]
  ): Either[
    ShortestPathAPSPError[V],
    UnweightedAPSPTrace[V, N, E]
  ] =
    prepare(samples, PlanNodeId.fresh())

  private[engine] def prepare[V, N, E](
      samples: Vector[GraphSample[V, N, E]],
      planNodeId: PlanNodeId
  ): Either[
    ShortestPathAPSPError[V],
    UnweightedAPSPTrace[V, N, E]
  ] =
    PackedGraphCorpus
      .pack(samples)
      .left
      .map(ShortestPathAPSPError.PackedCorpus(_))
      .flatMap { corpus =>
        val completed = Vector.newBuilder[UnweightedDistanceBlock]
        var matrixEntries = 0L
        var reachablePairs = 0L
        var failure: Option[ShortestPathAPSPError[V]] = None
        var graphIndex = 0
        while graphIndex < corpus.graphCount && failure.isEmpty do
          UnweightedDistanceBlock.fromGraph(corpus, graphIndex) match
            case Right((block, graphReachablePairs)) =>
              completed += block
              matrixEntries +=
                block.vertexCount.toLong * block.vertexCount.toLong
              reachablePairs += graphReachablePairs
            case Left(error) =>
              failure = Some(error)
          graphIndex += 1
        failure.toLeft(
          new UnweightedAPSPTrace(
            planNodeId,
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
