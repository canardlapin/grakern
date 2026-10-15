// This branch prepares only the JVM dependency artifacts listed in docs/dependency-release.md.
// Publishing is an explicit local staging operation; Central upload is a separate step.
ThisBuild / isSnapshot := false
ThisBuild / version := "0.1.0-RC1"
ThisBuild / homepage := Some(url("https://github.com/canardlapin/grakern"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/canardlapin/grakern"),
    "scm:git:https://github.com/canardlapin/grakern.git"
  )
)
ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := localStaging.value

addCommandAlias(
  "stageDependencyRelease",
  ";coreJVM/publish;graph4sAdapterJVM/publish;standardJVM/publish;lawsJVM/publish;engineJVM/publish"
)

ThisBuild / tlCiReleaseBranches := Seq.empty
ThisBuild / tlCiReleaseTags := false
