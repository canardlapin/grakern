package grakern.benchmark

import cats.kernel.{Eq, Hash}
import com.sun.management.ThreadMXBean
import grakern.core.*
import grakern.engine.WLCompiler
import grakern.standard.wl.*
import io.circe.{Decoder, Json}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode
import io.circe.syntax.*
import java.lang.management.{ManagementFactory, MemoryType}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Reproducible scaling receipt for the incremental sparse-query API.
  *
  * Run with `benchmarkWorker/runMain grakern.benchmark.QueryOverlayBench`. The prepared corpus has
  * 5,000 twenty-vertex graphs; the timed section submits 10,000 twenty-vertex queries. Per-thread
  * allocation excludes preparation and warmup, and the checksum keeps every sparse result live.
  */
object QueryOverlayBench:
  private val TrainingGraphs = 5_000
  private val Queries = 10_000
  private val Vertices = 20
  private val Warmups = 250

  given Hash[Int] = Hash.fromUniversalHashCode
  given Eq[Int] = Eq.fromUniversalEquals

  final private case class Arguments(
      output: Option[Path],
      baseline: Option[Path]
  )

  final private case class Baseline(
      expectedFeatureColumns: Int,
      expectedQuerySharedNnz: Int,
      expectedQueryCrossNnz: Int,
      expectedChecksum: Double,
      maximumAllocatedBytesPerQuery: Double,
      maximumPeakSingleQueryAllocatedBytes: Long
  )

  private object Baseline:
    given Decoder[Baseline] = deriveDecoder

  def main(rawArguments: Array[String]): Unit =
    val arguments = parseArguments(rawArguments)
    val training = Vector.tabulate(TrainingGraphs)(index => path(s"training-$index", index))
    val query = path("query", 0)
    val refinement =
      WLRefinement
        .create(
          1,
          Projection.of[Int, Int](identity),
          Projection.of[Int, Int](identity),
          WLCodecs[Int, Int](None, None)
        )
        .toOption
        .get
    val preparedStarted = System.nanoTime()
    val prepared =
      WLCompiler
        .plan(training, refinement.subtree)
        .run()
        .fold(
          error => throw new IllegalStateException(s"query benchmark preparation failed: $error"),
          identity
        )
    val preparedNanos = System.nanoTime() - preparedStarted

    var warmup = 0
    while warmup < Warmups do
      prepared
        .query(query)
        .fold(
          error => throw new IllegalStateException(s"query benchmark warmup failed: $error"),
          identity
        ): Unit
      warmup += 1

    val allocationBean =
      ManagementFactory.getThreadMXBean().asInstanceOf[ThreadMXBean]
    if allocationBean.isThreadAllocatedMemorySupported &&
      !allocationBean.isThreadAllocatedMemoryEnabled
    then allocationBean.setThreadAllocatedMemoryEnabled(true)
    val threadId = Thread.currentThread().getId
    val allocatedBefore = allocationBean.getThreadAllocatedBytes(threadId)
    var allocatedCursor = allocatedBefore
    var peakQueryAllocatedBytes = 0L
    val heapPools =
      ManagementFactory
        .getMemoryPoolMXBeans()
        .asScala
        .filter(pool => MemoryType.HEAP.equals(pool.getType))
        .toVector
    heapPools.foreach(_.resetPeakUsage())
    val heapBefore = heapPools.map(_.getUsage.getUsed).sum

    var checksum = 0.0
    var sharedNnz = 0
    var crossNnz = 0
    var index = 0
    val started = System.nanoTime()
    while index < Queries do
      val result = prepared
        .query(query)
        .fold(
          error => throw new IllegalStateException(s"query benchmark execution failed: $error"),
          identity
        )
      var entry = 0
      while entry < result.cross.values.size do
        checksum += result.cross.values(entry)
        entry += 1
      checksum += result.features.completeSquaredNorm
      sharedNnz = result.features.shared.activeSize
      crossNnz = result.cross.activeSize
      val allocatedNow = allocationBean.getThreadAllocatedBytes(threadId)
      peakQueryAllocatedBytes =
        math.max(peakQueryAllocatedBytes, math.max(0L, allocatedNow - allocatedCursor))
      allocatedCursor = allocatedNow
      index += 1
    val elapsedNanos = System.nanoTime() - started
    val allocatedAfter = allocationBean.getThreadAllocatedBytes(threadId)
    val heapPeak = heapPools.map(_.getPeakUsage.getUsed).sum
    val allocatedBytes = math.max(0L, allocatedAfter - allocatedBefore)
    val allocatedBytesPerQuery = allocatedBytes.toDouble / Queries.toDouble
    val featureColumns = prepared.features.columns.size

    arguments.baseline.foreach { path =>
      val baseline =
        decode[Baseline](Files.readString(path, StandardCharsets.UTF_8)).fold(
          error => throw new IllegalArgumentException(s"invalid query-overlay baseline: $error"),
          identity
        )
      require(
        featureColumns == baseline.expectedFeatureColumns,
        s"feature-column drift: $featureColumns != ${baseline.expectedFeatureColumns}"
      )
      require(
        sharedNnz == baseline.expectedQuerySharedNnz,
        s"shared-nnz drift: $sharedNnz != ${baseline.expectedQuerySharedNnz}"
      )
      require(
        crossNnz == baseline.expectedQueryCrossNnz,
        s"cross-nnz drift: $crossNnz != ${baseline.expectedQueryCrossNnz}"
      )
      require(
        checksum == baseline.expectedChecksum,
        s"checksum drift: $checksum != ${baseline.expectedChecksum}"
      )
      require(
        allocatedBytesPerQuery <= baseline.maximumAllocatedBytesPerQuery,
        s"allocation regression: $allocatedBytesPerQuery > ${baseline.maximumAllocatedBytesPerQuery} bytes/query"
      )
      require(
        peakQueryAllocatedBytes <= baseline.maximumPeakSingleQueryAllocatedBytes,
        s"peak query allocation regression: $peakQueryAllocatedBytes > ${baseline.maximumPeakSingleQueryAllocatedBytes}"
      )
    }

    val receipt = Json.obj(
      "schema" -> "grakern.query-overlay-benchmark.v1".asJson,
      "training_graphs" -> TrainingGraphs.asJson,
      "queries" -> Queries.asJson,
      "vertices_per_graph" -> Vertices.asJson,
      "rounds" -> 1.asJson,
      "feature_columns" -> featureColumns.asJson,
      "query_shared_nnz" -> sharedNnz.asJson,
      "query_cross_nnz" -> crossNnz.asJson,
      "prepared_nanos" -> preparedNanos.asJson,
      "elapsed_nanos" -> elapsedNanos.asJson,
      "nanos_per_query" -> (elapsedNanos.toDouble / Queries.toDouble).asJson,
      "thread_allocated_bytes" -> allocatedBytes.asJson,
      "allocated_bytes_per_query" -> allocatedBytesPerQuery.asJson,
      "peak_single_query_allocated_bytes" -> peakQueryAllocatedBytes.asJson,
      "heap_pool_peak_delta_bytes" -> math.max(0L, heapPeak - heapBefore).asJson,
      "checksum" -> checksum.asJson,
      "java_version" -> sys.props.getOrElse("java.version", "unknown").asJson,
      "os_name" -> sys.props.getOrElse("os.name", "unknown").asJson,
      "os_arch" -> sys.props.getOrElse("os.arch", "unknown").asJson,
      "processors" -> Runtime.getRuntime.availableProcessors().asJson
    )
    val rendered = receipt.spaces2
    arguments.output.foreach(path => Files.writeString(path, rendered, StandardCharsets.UTF_8))
    println(rendered)

  private def parseArguments(arguments: Array[String]): Arguments =
    var output = Option.empty[Path]
    var baseline = Option.empty[Path]
    var index = 0
    while index < arguments.length do
      arguments(index) match
        case "--output" if index + 1 < arguments.length =>
          output = Some(Path.of(arguments(index + 1)))
          index += 2
        case "--baseline" if index + 1 < arguments.length =>
          baseline = Some(Path.of(arguments(index + 1)))
          index += 2
        case other =>
          throw new IllegalArgumentException(
            s"unknown or incomplete query-overlay benchmark argument: $other"
          )
    Arguments(output, baseline)

  private def path(key: String, nodeLabel: Int): NeighbourhoodSample[Int, Int, Int] =
    val vertices = Vector.tabulate(Vertices)(identity)
    val edges = Vector.tabulate(Vertices - 1)(index => (index, index + 1, 0))
    NeighbourhoodSample
      .undirected(SampleKey(key), vertices, _ => nodeLabel, edges)
      .fold(
        error => throw new IllegalStateException(s"invalid benchmark graph: $error"),
        identity
      )
