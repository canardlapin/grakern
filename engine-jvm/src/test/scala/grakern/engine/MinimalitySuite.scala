package grakern.engine

import java.nio.file.{Files, Path, Paths}
import munit.FunSuite
import scala.jdk.CollectionConverters.*

/** ADR-009 boundary: `grakern-core` and `grakern-standard` main sources mention no graph4s symbol.
  * Only `grakern-graph4s` may import graph4s.
  */
final class MinimalitySuite extends FunSuite:
  private def repoRoot: Path =
    val candidates = Vector(Paths.get(""), Paths.get(".."), Paths.get("../.."))
    candidates
      .map(_.toAbsolutePath.normalize)
      .find(root => Files.isDirectory(root.resolve("core/src/main/scala")))
      .getOrElse(fail("could not locate the grakern repository root"))

  private def scalaSources(module: String): Vector[Path] =
    val dir = repoRoot.resolve(s"$module/src/main/scala")
    Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".scala")).toVector

  test("core and standard main sources reference no graph4s symbol"):
    val offenders =
      Vector("core", "standard").flatMap { module =>
        scalaSources(module).filter { path =>
          Files.readString(path).contains("graph4s")
        }
      }
    assertEquals(offenders, Vector.empty, clues(offenders))

  test("the protocol trait exposes exactly vertices, nodeValue, incidences, key, vertexHash"):
    val source =
      Files.readString(repoRoot.resolve("core/src/main/scala/grakern/core/Neighbourhood.scala"))
    val start = source.indexOf("trait LabelledNeighbourhood")
    val end = source.indexOf("enum NeighbourhoodError")
    val body = source.substring(start, end)
    val members = body.linesIterator
      .map(_.trim)
      .filter(_.startsWith("def "))
      .map(_.drop(4).takeWhile(c => c != '(' && c != ':'))
      .toVector
    assertEquals(members, Vector("key", "vertexHash", "vertices", "nodeValue", "incidences"))
