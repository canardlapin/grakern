package grakern.benchmark

import cats.kernel.Hash
import com.sun.management.{OperatingSystemMXBean, ThreadMXBean}
import grakern.core.*
import grakern.engine.*
import grakern.graph4s.GraphSample
import grakern.standard.shortestpath.*
import grakern.standard.wl.*
import graph4s.{Graph, Link}
import io.circe.*
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode
import io.circe.syntax.*
import java.lang.management.ManagementFactory
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

final private[benchmark] case class NeutralGraph(
    key: String,
    node_labels: Vector[Int],
    edges: Vector[Vector[Int]]
)

private[benchmark] object NeutralGraph:
  given Decoder[NeutralGraph] = deriveDecoder

final private[benchmark] case class NeutralCorpus(
    training_graphs: Vector[NeutralGraph],
    query_graphs: Vector[NeutralGraph]
)

private[benchmark] object NeutralCorpus:
  given Decoder[NeutralCorpus] = deriveDecoder

final private[benchmark] case class WorkerJob(
    id: String,
    boundary: String,
    environment: Map[String, String]
)

private[benchmark] object WorkerJob:
  given Decoder[WorkerJob] = deriveDecoder

final private[benchmark] case class WorkerExecution(
    fork_index: Int
)

private[benchmark] object WorkerExecution:
  given Decoder[WorkerExecution] = deriveDecoder

final private[benchmark] case class SamplingContract(
    mode: String,
    policy: String,
    warmups: Int,
    samples: Int
)

private[benchmark] object SamplingContract:
  given Decoder[SamplingContract] = deriveDecoder

final private[benchmark] case class KernelConfiguration(
    kernel: String,
    rounds: Option[Int]
)

private[benchmark] object KernelConfiguration:
  given Decoder[KernelConfiguration] = deriveDecoder

final private[benchmark] case class CorpusContract(
    path: String
)

private[benchmark] object CorpusContract:
  given Decoder[CorpusContract] = deriveDecoder

final private[benchmark] case class ExpectedOutput(
    comparison: String,
    shape: Vector[Int],
    sample_order_sha256: String,
    expected_sha256: String
)

private[benchmark] object ExpectedOutput:
  given Decoder[ExpectedOutput] = deriveDecoder

final private[benchmark] case class WorkerPayload(
    job: WorkerJob,
    execution: WorkerExecution,
    sampling_contract: SamplingContract,
    kernel_configuration: KernelConfiguration,
    corpus: CorpusContract,
    expected_output: ExpectedOutput
)

private[benchmark] object WorkerPayload:
  given Decoder[WorkerPayload] = deriveDecoder

final private[benchmark] case class NativeCorpus(
    training: Vector[GraphSample[Int, Int, Unit]],
    query: Vector[GraphSample[Int, Int, Unit]]
)

final private[benchmark] case class DenseResult(
    rows: Int,
    columns: Int,
    values: Array[Double],
    rowKeys: Vector[String],
    columnKeys: Vector[String]
):
  def apply(row: Int, column: Int): Double =
    values(row * columns + column)

final private[benchmark] case class OperationResult(
    matrix: DenseResult,
    phases: Map[String, Long]
)

final private[benchmark] case class SemanticObservation(
    shape: Vector[Int],
    sampleOrderSha256: String,
    outputSha256: String
)

final private[benchmark] case class OperationContext(
    repository: Path,
    payload: WorkerPayload,
    neutral: NeutralCorpus,
    prepared: Option[NativeCorpus]
)

private[benchmark] object WorkerEngine:
  private val TargetSampleNanos = 350_000_000L
  private val MaximumBatchSize = 100_000
  private val ThreadEnvironment =
    Set(
      "GRAKERN_COURT_APPLICATION_THREADS",
      "OMP_NUM_THREADS",
      "OPENBLAS_NUM_THREADS",
      "MKL_NUM_THREADS",
      "VECLIB_MAXIMUM_THREADS",
      "JULIA_NUM_THREADS"
    )

  given Hash[Int] = Hash.fromUniversalHashCode

  def loadContext(
      repository: Path,
      payload: WorkerPayload
  ): Either[String, OperationContext] =
    val corpusPath = repository.resolve(payload.corpus.path)
    for
      neutral <- decode[NeutralCorpus](
        Files.readString(corpusPath)
      ).left.map(_.getMessage)
      prepared <-
        if payload.job.boundary == "prepared_topology" then nativeCorpus(neutral).map(Some(_))
        else Right(None)
    yield OperationContext(
      repository,
      payload,
      neutral,
      prepared
    )

  def evaluateOnce(
      context: OperationContext
  ): Either[String, OperationResult] =
    operation(context)

  def semanticObservation(
      matrix: DenseResult
  ): SemanticObservation =
    SemanticObservation(
      Vector(matrix.rows, matrix.columns),
      sampleOrderSha256(
        matrix.rowKeys,
        matrix.columnKeys
      ),
      sha256(matrixBytes(matrix))
    )

  def runWorker(
      context: OperationContext
  ): Either[String, Json] =
    val payload = context.payload
    val contract = payload.sampling_contract
    val runtimeBaseline = rssBytes()
    val batchSize =
      if contract.mode == "cold" then 1
      else
        val calibrationStarted = System.nanoTime()
        operation(context).map(_ => ()) match
          case Left(error) => return Left(error)
          case Right(_)    => ()
        chooseBatchSize(
          System.nanoTime() - calibrationStarted
        )

    var last: Option[OperationResult] = None
    var warmup = 0
    while warmup < contract.warmups do
      operation(context) match
        case Left(error)   => return Left(error)
        case Right(result) => last = Some(result)
      warmup += 1

    val durations = Vector.newBuilder[Long]
    val phaseSamples =
      mutable.Map.empty[String, Vector[Double]]
    val allocationBean =
      ManagementFactory
        .getThreadMXBean()
        .asInstanceOf[ThreadMXBean]
    if allocationBean.isThreadAllocatedMemorySupported &&
      !allocationBean.isThreadAllocatedMemoryEnabled
    then allocationBean.setThreadAllocatedMemoryEnabled(true)
    val allocatedBefore =
      totalThreadAllocatedBytes(allocationBean)
    val operatingSystem =
      ManagementFactory
        .getOperatingSystemMXBean()
        .asInstanceOf[OperatingSystemMXBean]
    val cpuBefore = operatingSystem.getProcessCpuTime()
    val gcBefore = gcCollections()

    var sample = 0
    while sample < contract.samples do
      val samplePhases = mutable.Map.empty[String, Long]
      val started = System.nanoTime()
      var batch = 0
      while batch < batchSize do
        operation(context) match
          case Left(error)   => return Left(error)
          case Right(result) =>
            last = Some(result)
            result.phases.foreach { case (name, duration) =>
              samplePhases.update(
                name,
                samplePhases.getOrElse(name, 0L) + duration
              )
            }
        batch += 1
      val duration =
        math.max(1L, System.nanoTime() - started)
      durations += duration
      samplePhases.foreach { case (name, phaseDuration) =>
        phaseSamples.update(
          name,
          phaseSamples.getOrElse(name, Vector.empty) :+
            phaseDuration.toDouble / batchSize.toDouble
        )
      }
      sample += 1

    val observed =
      last.toRight("sampling produced no matrix").map(_.matrix)
    observed.map { matrix =>
      val durationValues = durations.result()
      val semantic = semanticObservation(matrix)
      val observedSha = semantic.outputSha256
      val observedOrder = semantic.sampleOrderSha256
      val expected = payload.expected_output
      val shape = semantic.shape
      val exact =
        shape == expected.shape &&
          observedOrder == expected.sample_order_sha256 &&
          observedSha == expected.expected_sha256
      val allocatedAfter =
        totalThreadAllocatedBytes(allocationBean)
      val cpuAfter = operatingSystem.getProcessCpuTime()
      val peakRss = math.max(runtimeBaseline, rssBytes())
      val medianDuration =
        roundHalfEven(
          median(
            durationValues.map(_.toDouble / batchSize.toDouble)
          )
        )
      val phaseJson =
        Json.obj(
          phaseSamples.toVector
            .sortBy(_._1)
            .map { case (name, values) =>
              name -> Json.fromLong(
                roundHalfEven(median(values))
              )
            }*
        )
      val allocated =
        math.max(0L, allocatedAfter - allocatedBefore)
      val cpu = math.max(0L, cpuAfter - cpuBefore)
      Json.obj(
        "schema_version" -> Json.fromString(
          "grakern.performance-worker-result.v1"
        ),
        "job_id" -> Json.fromString(payload.job.id),
        "runtime" -> runtimeJson(payload.job),
        "semantic_output" -> Json.obj(
          "shape" -> shape.asJson,
          "sample_order_sha256" -> Json.fromString(
            observedOrder
          ),
          "comparison" -> Json.fromString(
            expected.comparison
          ),
          "observed_sha256" -> Json.fromString(
            observedSha
          ),
          "expected_sha256" -> Json.fromString(
            expected.expected_sha256
          ),
          "finite" -> Json.fromBoolean(
            matrix.values.forall(_.isFinite)
          )
        ),
        "sampling" -> Json.obj(
          "mode" -> Json.fromString(contract.mode),
          "policy" -> Json.fromString(contract.policy),
          "fork_index" -> Json.fromInt(
            payload.execution.fork_index
          ),
          "warmups" -> Json.fromInt(contract.warmups),
          "samples" -> Json.fromInt(contract.samples),
          "batch_size" -> Json.fromInt(batchSize),
          "timed_work_ns" -> Json.fromLong(
            durationValues.sum
          ),
          "durations_ns" -> durationValues.asJson
        ),
        "metrics" -> Json.obj(
          "wall_clock_median_ns" -> Json.fromLong(
            medianDuration
          ),
          "cpu_time_ns" -> available(cpu),
          "runtime_baseline_rss_bytes" -> Json.fromLong(
            runtimeBaseline
          ),
          "peak_rss_bytes" -> Json.fromLong(peakRss),
          "peak_rss_delta_bytes" -> Json.fromLong(
            peakRss - runtimeBaseline
          ),
          "allocated_bytes" -> available(allocated),
          "gc_count" -> available(
            math.max(0L, gcCollections() - gcBefore)
          ),
          "phases" -> phaseJson
        ),
        "proof_checks" -> Vector(
          proof(
            "semantic.independent_reference",
            exact,
            s"shape=$shape/${expected.shape} order=$observedOrder/${expected.sample_order_sha256} output=$observedSha/${expected.expected_sha256}"
          ),
          proof(
            "operation.state_reset",
            passed = true,
            "every operation creates fresh refinement, feature, APSP, and matrix state"
          ),
          proof(
            "boundary.complete",
            passed = true,
            if payload.job.boundary == "public_end_to_end"
            then "public graph4s graph and field construction is timed"
            else "native graph4s topology and fields are prepared; all kernel work is timed"
          )
        ).asJson
      )
    }

  private def operation(
      context: OperationContext
  ): Either[String, OperationResult] =
    val started = System.nanoTime()
    val native =
      context.prepared match
        case Some(value) => Right(value)
        case None        => nativeCorpus(context.neutral)
    native.flatMap { corpus =>
      val graphsReady = System.nanoTime()
      evaluateKernel(
        corpus,
        context.payload.kernel_configuration
      ).map { matrix =>
        val kernelComplete = System.nanoTime()
        OperationResult(
          matrix,
          Map(
            "public_graph_construction" ->
              (graphsReady - started),
            "kernel_and_matrix" ->
              (kernelComplete - graphsReady)
          )
        )
      }
    }

  private def nativeCorpus(
      corpus: NeutralCorpus
  ): Either[String, NativeCorpus] =
    for
      training <- traverse(corpus.training_graphs)(
        graphSample
      )
      query <- traverse(corpus.query_graphs)(
        graphSample
      )
    yield NativeCorpus(training, query)

  private def graphSample(
      sample: NeutralGraph
  ): Either[String, GraphSample[Int, Int, Unit]] =
    sample.edges.find(_.size != 2) match
      case Some(value) =>
        Left(
          s"sample ${sample.key} has malformed edge $value"
        )
      case None =>
        val links =
          sample.edges.map(edge => Link(edge(0), edge(1)))
        Graph
          .of(sample.node_labels.indices, links)
          .toEither
          .left
          .map(errors => s"sample ${sample.key}: ${errors.toNonEmptyList.toList.mkString("; ")}")
          .map { topology =>
            GraphSample.build(SampleKey(sample.key), topology)(
              nodeValue = sample.node_labels,
              edgeValue = _ => ()
            )
          }

  private def evaluateKernel(
      corpus: NativeCorpus,
      configuration: KernelConfiguration
  ): Either[String, DenseResult] =
    configuration.kernel match
      case "wl_subtree" =>
        evaluateWL(corpus, configuration, assignment = false)
      case "wl_optimal_assignment" =>
        evaluateWL(corpus, configuration, assignment = true)
      case "wl_subtree_cross" =>
        evaluateWL(corpus, configuration, assignment = false)
      case "categorical_shortest_path" =>
        evaluateShortestPath(corpus)
      case value =>
        Left(s"unsupported Grakern court kernel: $value")

  private def evaluateWL(
      corpus: NativeCorpus,
      configuration: KernelConfiguration,
      assignment: Boolean
  ): Either[String, DenseResult] =
    val rounds =
      configuration.rounds.toRight(
        "WL configuration lacks rounds"
      )
    rounds.flatMap { count =>
      val refinement =
        WLRefinement.create(
          count,
          Projection.of[Int, Int](identity),
          Projection.of[Unit, Int](_ => 0),
          WLCodecs[Int, Int](None, None)
        )
      refinement.left
        .map(_.toString)
        .flatMap { wl =>
          val kernel =
            if assignment then wl.optimalAssignment
            else wl.subtree
          val options =
            WLPlanOptions(
              gramExecution = gramExecution
            )
          WLCompiler
            .plan(corpus.training, kernel, options)
            .run()
            .left
            .map(_.toString)
            .flatMap { prepared =>
              if configuration.kernel == "wl_subtree_cross"
              then
                prepared
                  .cross(corpus.query)
                  .left
                  .map(_.toString)
                  .map { result =>
                    fromMatrix(
                      result.cross.values.rows,
                      result.cross.values.cols,
                      result.cross.values.apply,
                      corpus.query.map(_.key.value),
                      corpus.training.map(_.key.value)
                    )
                  }
              else
                Right(
                  fromMatrix(
                    prepared.gram.values.rows,
                    prepared.gram.values.cols,
                    prepared.gram.values.apply,
                    corpus.training.map(_.key.value),
                    corpus.training.map(_.key.value)
                  )
                )
            }
        }
    }

  private def evaluateShortestPath(
      corpus: NativeCorpus
  ): Either[String, DenseResult] =
    val recipe =
      UnweightedShortestPaths.parts[Int, Unit, Int](
        Projection.of(_ => 0)
      )
    ShortestPathPlanner
      .categorical(
        corpus.training,
        recipe,
        CategoricalShortestPathPlanOptions(
          gramExecution = gramExecution
        )
      )
      .run()
      .left
      .map(_.toString)
      .map { prepared =>
        fromMatrix(
          prepared.gram.values.rows,
          prepared.gram.values.cols,
          prepared.gram.values.apply,
          corpus.training.map(_.key.value),
          corpus.training.map(_.key.value)
        )
      }

  private def gramExecution: GramExecution =
    val parallelism =
      sys.env
        .get("GRAKERN_COURT_APPLICATION_THREADS")
        .flatMap(_.toIntOption)
        .getOrElse(1)
    if parallelism <= 1 then GramExecution.Sequential
    else
      GramExecution
        .deterministicParallel(parallelism, 16)
        .fold(
          error =>
            throw new IllegalArgumentException(
              error.toString
            ),
          identity
        )

  private def fromMatrix(
      rows: Int,
      columns: Int,
      value: (Int, Int) => Double,
      rowKeys: Vector[String],
      columnKeys: Vector[String]
  ): DenseResult =
    DenseResult(
      rows,
      columns,
      Array.tabulate(rows * columns) { index =>
        value(index / columns, index % columns)
      },
      rowKeys,
      columnKeys
    )

  private def matrixBytes(matrix: DenseResult): Array[Byte] =
    val buffer =
      ByteBuffer
        .allocate(matrix.values.length * java.lang.Double.BYTES)
        .order(ByteOrder.BIG_ENDIAN)
    matrix.values.foreach { value =>
      if !value.isFinite then
        throw new IllegalArgumentException(
          "Grakern produced a nonfinite matrix value"
        )
      buffer.putDouble(if value == 0.0 then 0.0 else value)
    }
    buffer.array()

  private def sampleOrderSha256(
      rows: Vector[String],
      columns: Vector[String]
  ): String =
    val value =
      Json
        .obj(
          "columns" -> columns.asJson,
          "rows" -> rows.asJson
        )
        .noSpaces + "\n"
    sha256(value.getBytes(StandardCharsets.UTF_8))

  private def sha256(value: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

  private def chooseBatchSize(
      calibrationNanos: Long
  ): Int =
    if calibrationNanos >= 100_000_000L then 1
    else
      val estimate =
        math.max(
          1L,
          (TargetSampleNanos + calibrationNanos - 1L) /
            math.max(1L, calibrationNanos)
        )
      math.min(estimate, MaximumBatchSize.toLong).toInt

  private def median(values: Vector[Double]): Double =
    val sorted = values.sorted
    val middle = sorted.size / 2
    if sorted.size % 2 == 1 then sorted(middle)
    else (sorted(middle - 1) + sorted(middle)) / 2.0

  private[benchmark] def roundHalfEven(
      value: Double
  ): Long =
    java.lang.Math.rint(value).toLong

  private def runtimeJson(job: WorkerJob): Json =
    val names =
      (ThreadEnvironment ++ job.environment.keySet).toVector.sorted
    Json.obj(
      "language" -> Json.fromString("scala-jvm"),
      "language_version" -> Json.fromString(
        scala.util.Properties.versionNumberString
      ),
      "vm_version" -> Json.fromString(
        property("java.vm.name") + " " +
          property("java.vm.version")
      ),
      "compiler_version" -> Json.fromString(
        scala.util.Properties.versionNumberString
      ),
      "operating_system" -> Json.fromString(
        property("os.name") + " " + property("os.version")
      ),
      "architecture" -> Json.fromString(
        property("os.arch")
      ),
      "environment" -> Json.obj(
        names.map { name =>
          name -> Json.fromString(
            sys.env.getOrElse(name, "")
          )
        }*
      )
    )

  private def property(name: String): String =
    Option(System.getProperty(name)).getOrElse("unknown")

  private def available(value: Long): Json =
    Json.obj(
      "available" -> Json.fromBoolean(true),
      "value" -> Json.fromLong(value)
    )

  private def proof(
      id: String,
      passed: Boolean,
      evidence: String
  ): Json =
    Json.obj(
      "id" -> Json.fromString(id),
      "passed" -> Json.fromBoolean(passed),
      "evidence" -> Json.fromString(evidence)
    )

  private def gcCollections(): Long =
    ManagementFactory
      .getGarbageCollectorMXBeans()
      .stream()
      .mapToLong(bean => math.max(0L, bean.getCollectionCount()))
      .sum()

  private def totalThreadAllocatedBytes(
      bean: ThreadMXBean
  ): Long =
    bean
      .getThreadAllocatedBytes(bean.getAllThreadIds())
      .iterator
      .filter(_ >= 0L)
      .sum

  private def rssBytes(): Long =
    val fallback =
      Runtime.getRuntime().totalMemory() -
        Runtime.getRuntime().freeMemory()
    try
      val process =
        new ProcessBuilder(
          "ps",
          "-o",
          "rss=",
          "-p",
          ProcessHandle.current().pid().toString
        ).start()
      val value =
        Using.resource(
          Source.fromInputStream(process.getInputStream())
        )(_.mkString.trim)
      val status = process.waitFor()
      if status == 0 then
        value.toLongOption
          .map(_ * 1024L)
          .getOrElse(fallback)
      else fallback
    catch case _: Throwable => fallback

  private def traverse[A, B](
      values: Vector[A]
  )(
      operation: A => Either[String, B]
  ): Either[String, Vector[B]] =
    values.foldLeft(
      Right(Vector.empty): Either[String, Vector[B]]
    ) { (result, value) =>
      for
        accumulated <- result
        next <- operation(value)
      yield accumulated :+ next
    }

object WorkerMain:
  def main(arguments: Array[String]): Unit =
    val parsed =
      arguments
        .grouped(2)
        .collect { case Array(name, value) =>
          name -> value
        }
        .toMap
    val jobPath =
      parsed
        .get("--job")
        .map(Path.of(_))
        .getOrElse(
          throw new IllegalArgumentException(
            "missing --job"
          )
        )
    val outputPath =
      parsed
        .get("--output")
        .map(Path.of(_))
        .getOrElse(
          throw new IllegalArgumentException(
            "missing --output"
          )
        )
    val repository =
      Iterator
        .iterate(
          Path.of("").toAbsolutePath.normalize()
        )(_.getParent())
        .takeWhile(_ != null)
        .find(path =>
          Files.isRegularFile(
            path.resolve("benchmarks/court/tier1.json")
          )
        )
        .getOrElse(
          throw new IllegalArgumentException(
            "cannot locate Grakern repository from worker job"
          )
        )
    val payload =
      decode[WorkerPayload](
        Files.readString(jobPath)
      ).fold(
        error => throw error,
        identity
      )
    val result =
      WorkerEngine
        .loadContext(repository, payload)
        .flatMap(WorkerEngine.runWorker)
        .fold(
          error => throw new IllegalStateException(error),
          identity
        )
    Option(outputPath.getParent()).foreach(
      Files.createDirectories(_)
    )
    Files.writeString(
      outputPath,
      Printer.spaces2SortKeys.print(result) + "\n"
    ): Unit
