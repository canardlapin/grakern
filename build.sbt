import org.typelevel.sbt.gha.JavaSpec
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

val Scala3           = "3.7.4"
val CatsV            = "2.13.0"
val MunitV           = "1.3.4"
val MunitScalaCheckV = "1.3.0"
val DisciplineMunitV = "2.0.0"
val ScalaCheckV      = "1.19.0"

val Graph4sRevision = "ea5d2d762f85f5a0f97ee188deb5fac0ef2bcbaf"
val GaleRevision    = "d55fe2f97196a76ab7879e1a12f1e92403aeba06"

ThisBuild / tlBaseVersion    := "0.1"
ThisBuild / organization     := "io.github.canardlapin"
ThisBuild / organizationName := "Bradley Buchsbaum"
ThisBuild / startYear        := Some(2026)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(
  tlGitHubDev("canardlapin", "Bradley Buchsbaum")
)

ThisBuild / scalaVersion       := Scala3
ThisBuild / crossScalaVersions := Seq(Scala3)
ThisBuild / tlJdkRelease       := Some(17)
ThisBuild / githubWorkflowJavaVersions := Seq(
  JavaSpec.temurin("21")
)

lazy val strictSettings = Seq(
  scalacOptions ++= Seq(
    "-Wunused:all",
    "-Yexplicit-nulls",
    "-language:strictEquality",
    "-Werror"
  ),
  Test / parallelExecution := false,
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit"            % MunitV           % Test,
    "org.scalameta" %%% "munit-scalacheck" % MunitScalaCheckV % Test
  )
)

// Unreleased dependencies are fetched from clean, immutable revisions. A local
// sibling checkout is never part of the reproducible build contract.
lazy val graph4sBuild = uri(
  s"https://github.com/canardlapin/graph4s.git#$Graph4sRevision"
)
lazy val graph4sCoreJVM    = ProjectRef(graph4sBuild, "coreJVM")
lazy val graph4sCoreJS     = ProjectRef(graph4sBuild, "coreJS")
lazy val graph4sCoreNative = ProjectRef(graph4sBuild, "coreNative")
lazy val graph4sIndexedJVM    = ProjectRef(graph4sBuild, "indexedJVM")
lazy val graph4sIndexedJS     = ProjectRef(graph4sBuild, "indexedJS")
lazy val graph4sIndexedNative = ProjectRef(graph4sBuild, "indexedNative")
lazy val graph4sDataJVM    = ProjectRef(graph4sBuild, "dataJVM")
lazy val graph4sDataJS     = ProjectRef(graph4sBuild, "dataJS")
lazy val graph4sDataNative = ProjectRef(graph4sBuild, "dataNative")

lazy val galeBuild   = uri(s"https://github.com/canardlapin/gale.git#$GaleRevision")
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")

lazy val root = tlCrossRootProject
  .aggregate(core, graph4sAdapter, standard, laws)
  .aggregate(engineJVM, architectureProbes)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(strictSettings)
  .settings(
    name := "grakern-core",
    libraryDependencies += "org.typelevel" %%% "cats-kernel" % CatsV
  )

lazy val graph4sAdapter = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("graph4s"))
  .dependsOn(core)
  .jvmConfigure(_.dependsOn(graph4sCoreJVM, graph4sIndexedJVM, graph4sDataJVM))
  .jsConfigure(_.dependsOn(graph4sCoreJS, graph4sIndexedJS, graph4sDataJS))
  .nativeConfigure(
    _.dependsOn(graph4sCoreNative, graph4sIndexedNative, graph4sDataNative)
  )
  .settings(strictSettings)
  .settings(name := "grakern-graph4s")

lazy val standard = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("standard"))
  .dependsOn(core, graph4sAdapter)
  .settings(strictSettings)
  .settings(name := "grakern-standard")

lazy val laws = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("laws"))
  .dependsOn(core, graph4sAdapter, standard)
  .settings(strictSettings)
  .settings(
    name := "grakern-laws",
    libraryDependencies ++= Seq(
      "org.scalameta"  %%% "munit"            % MunitV,
      "org.typelevel"  %%% "discipline-munit" % DisciplineMunitV,
      "org.scalacheck" %%% "scalacheck"       % ScalaCheckV
    )
  )

lazy val engineJVM = project
  .in(file("engine-jvm"))
  .dependsOn(core.jvm, graph4sAdapter.jvm, standard.jvm, galeCoreJVM)
  .settings(strictSettings)
  .settings(name := "grakern-engine-jvm")

lazy val architectureProbes = project
  .in(file("architecture-probes"))
  .dependsOn(core.jvm, graph4sAdapter.jvm, standard.jvm)
  .settings(strictSettings)
  .settings(
    name           := "grakern-architecture-probes",
    publish / skip := true
  )

addCommandAlias(
  "compileAll",
  (
    List("core", "graph4sAdapter", "standard", "laws")
      .flatMap(module =>
        List("JVM", "JS", "Native").map(platform => s"$module$platform/compile")
      ) ++
      List("engineJVM/compile", "architectureProbes/compile")
  ).mkString(";", ";", "")
)

addCommandAlias(
  "testAll",
  (
    List("core", "graph4sAdapter", "standard", "laws")
      .flatMap(module =>
        List("JVM", "JS", "Native").map(platform => s"$module$platform/test")
      ) ++
      List("engineJVM/test", "architectureProbes/test")
  ).mkString(";", ";", "")
)
