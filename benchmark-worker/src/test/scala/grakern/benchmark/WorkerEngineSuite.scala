package grakern.benchmark

import io.circe.*
import io.circe.parser.*
import java.nio.file.{Files, Path}
import munit.FunSuite

final class WorkerEngineSuite extends FunSuite:
  test("standard batching targets 120 ms without batching 100 ms operations") {
    assertEquals(WorkerEngine.chooseBatchSize(1_000_000L), 120)
    assertEquals(WorkerEngine.chooseBatchSize(99_000_000L), 2)
    assertEquals(WorkerEngine.chooseBatchSize(100_000_000L), 1)
  }

  private val repository =
    Iterator
      .iterate(Path.of("").toAbsolutePath)(_.getParent())
      .takeWhile(_ != null)
      .find(path =>
        Files.isRegularFile(
          path.resolve("benchmarks/court/tier1.json")
        )
      )
      .get

  private val tier1 =
    parse(
      Files.readString(
        repository.resolve("benchmarks/court/tier1.json")
      )
    ).toOption.get

  private def objectAt(
      cursor: HCursor,
      field: String,
      key: String
  ): Json =
    cursor
      .downField(field)
      .downField(key)
      .focus
      .get

  private def payload(
      row: Json,
      boundary: String
  ): WorkerPayload =
    payloadJson(row, boundary)
      .as[WorkerPayload]
      .toOption
      .get

  private def payloadJson(
      row: Json,
      boundary: String
  ): Json =
    val cursor = row.hcursor
    val kernel = cursor.get[String]("kernel").toOption.get
    val corpus = cursor.get[String]("corpus").toOption.get
    val job =
      Json.obj(
        "id" -> Json.fromString(
          s"test-${cursor.get[String]("id").toOption.get}-$boundary"
        ),
        "boundary" -> Json.fromString(boundary),
        "environment" -> Json.obj()
      )
    val value =
      Json.obj(
        "job" -> job,
        "execution" -> Json.obj(
          "fork_index" -> Json.fromInt(0)
        ),
        "sampling_contract" -> Json.obj(
          "mode" -> Json.fromString("cold"),
          "policy" -> Json.fromString("standard"),
          "warmups" -> Json.fromInt(0),
          "samples" -> Json.fromInt(1)
        ),
        "kernel_configuration" -> objectAt(
          tier1.hcursor,
          "kernel_configurations",
          kernel
        ),
        "corpus" -> objectAt(
          tier1.hcursor,
          "corpora",
          corpus
        ),
        "expected_output" ->
          cursor.downField("expected_output").focus.get
      )
    value

  private val rows =
    tier1.hcursor
      .get[Vector[Json]]("rows")
      .toOption
      .get

  private def assertRow(
      row: Json,
      boundary: String
  ): Unit =
    val context =
      WorkerEngine
        .loadContext(
          repository,
          payload(row, boundary)
        )
        .toOption
        .get
    val result =
      WorkerEngine.evaluateOnce(context).toOption.get
    val expected =
      row.hcursor.downField("expected_output")
    val semantic =
      WorkerEngine.semanticObservation(result.matrix)
    assertEquals(
      semantic.shape,
      expected.get[Vector[Int]]("shape").toOption.get,
      clues(
        row.hcursor.get[String]("id").toOption,
        boundary
      )
    )
    assertEquals(
      semantic.sampleOrderSha256,
      expected
        .get[String]("sample_order_sha256")
        .toOption
        .get
    )
    assertEquals(
      semantic.outputSha256,
      expected
        .get[String]("expected_sha256")
        .toOption
        .get
    )

  rows.foreach { row =>
    val id = row.hcursor.get[String]("id").toOption.get
    test(s"Tier-1 prepared parity: $id"):
      assertRow(row, "prepared_topology")
  }

  test("public boundary rebuilds graph4s objects without semantic drift"):
    assertRow(rows.head, "public_end_to_end")

  test("worker result carries the required semantic proofs"):
    val row = rows.head
    val context =
      WorkerEngine
        .loadContext(
          repository,
          payload(row, "prepared_topology")
        )
        .toOption
        .get
    val result =
      WorkerEngine.runWorker(context).toOption.get
    val checks =
      result.hcursor
        .get[Vector[Json]]("proof_checks")
        .toOption
        .get
    assert(
      checks.forall(
        _.hcursor.get[Boolean]("passed").contains(true)
      )
    )
    assertEquals(
      result.hcursor
        .get[String]("schema_version")
        .toOption,
      Some("grakern.performance-worker-result.v1")
    )

  test("protocol rounding uses ties to even"):
    assertEquals(WorkerEngine.roundHalfEven(2.5), 2L)
    assertEquals(WorkerEngine.roundHalfEven(3.5), 4L)

  test("CLI accepts court artifacts outside the source checkout"):
    val directory =
      Files.createTempDirectory("grakern-worker-cli-")
    val job = directory.resolve("job.json")
    val output = directory.resolve("worker-result.json")
    try
      Files.writeString(
        job,
        Printer.spaces2SortKeys.print(
          payloadJson(rows.head, "prepared_topology")
        )
      )
      WorkerMain.main(
        Array(
          "--job",
          job.toString,
          "--output",
          output.toString
        )
      )
      val result =
        parse(Files.readString(output)).toOption.get
      assertEquals(
        result.hcursor
          .get[String]("schema_version")
          .toOption,
        Some("grakern.performance-worker-result.v1")
      )
      assert(
        result.hcursor
          .get[Vector[Json]]("proof_checks")
          .toOption
          .get
          .forall(
            _.hcursor.get[Boolean]("passed").contains(true)
          )
      )
    finally
      Files.deleteIfExists(output)
      Files.deleteIfExists(job)
      Files.deleteIfExists(directory)
      ()
