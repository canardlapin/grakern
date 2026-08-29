package grakern.architecture.probe

import java.nio.file.{Files, Path}
import java.util.Properties
import munit.FunSuite
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

final class ReleaseReceiptSuite extends FunSuite:
  private val repository =
    Iterator
      .iterate(Path.of("").toAbsolutePath.normalize)(_.getParent)
      .takeWhile(_ != null)
      .find(path => Files.isRegularFile(path.resolve("release/0.1.0-M1.properties")))
      .getOrElse(fail("could not locate the Grakern repository root"))

  private val receiptPath = repository.resolve("release/0.1.0-M1.properties")
  private val receipt =
    val result = new Properties()
    val input = Files.newInputStream(receiptPath)
    try result.load(input)
    finally input.close()
    result

  private val build = Files.readString(repository.resolve("build.sbt"))

  private def property(name: String): String =
    Option(receipt.getProperty(name)).getOrElse(fail(s"missing release receipt property: $name"))

  private def buildRevision(name: String): String =
    val assignment: Regex = raw"val\s+$name\s*=\s*\"([0-9a-f]{40})\"".r
    assignment
      .findFirstMatchIn(build)
      .map(_.group(1))
      .getOrElse(fail(s"build.sbt has no exact full-SHA assignment for $name"))

  test("the milestone receipt identifies its target and toolchain"):
    assertEquals(property("schema.version"), "grakern.release-pin-receipt.v1")
    assertEquals(property("release.version"), "0.1.0-M1")
    assertEquals(property("release.stage"), "milestone")
    assertEquals(property("scala.version"), "3.7.4")
    assertEquals(property("jdk.release"), "17")

  test("the transitive source-pin receipt cannot drift from build.sbt"):
    assertEquals(property("graph4s.revision"), buildRevision("Graph4sRevision"))
    assertEquals(property("gale.revision"), buildRevision("GaleRevision"))
    assertEquals(property("graph4s.resolution"), "source-pin")
    assertEquals(property("gale.resolution"), "source-pin")

  test("admitted and excluded module sets are explicit and disjoint"):
    val admitted = property("admitted.modules").split(',').toSet
    val excluded = property("excluded.modules").split(',').toSet
    assertEquals(
      admitted,
      Set(
        "grakern-core",
        "grakern-graph4s",
        "grakern-standard",
        "grakern-laws",
        "grakern-engine-jvm"
      )
    )
    assertEquals(
      excluded,
      Set(
        "grakern-architecture-probes",
        "grakern-examples-jvm",
        "grakern-benchmark-worker"
      )
    )
    assertEquals(admitted.intersect(excluded), Set.empty)

  test("a source-pinned receipt remains visibly dependency-blocked"):
    val resolutionModes =
      receipt
        .stringPropertyNames()
        .asScala
        .filter(_.endsWith(".resolution"))
        .map(property)
        .toSet
    assert(resolutionModes.contains("source-pin"))
    assertEquals(property("release.readiness"), "dependency-blocked")
