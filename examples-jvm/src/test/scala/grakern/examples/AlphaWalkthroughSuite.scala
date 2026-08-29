package grakern.examples

import munit.FunSuite

final class AlphaWalkthroughSuite extends FunSuite:
  test("the documented WL preparation, query, identity, and centering workflow executes"):
    val result = AlphaWalkthrough.wl.toOption.get

    assertEquals(result.planHeading, "WL execution plan")
    assert(result.featureCount > 0)
    assert(result.trainingSimilarity > 0.0)
    assert(result.querySimilarity > 0.0)
    assertEquals(
      java.lang.Double.doubleToLongBits(result.queryOnlySimilarity),
      java.lang.Double.doubleToLongBits(result.querySimilarity)
    )
    assert(result.queryNovelSquaredNorm > 0.0)
    assert(result.centeredQuerySimilarity.isFinite)
    assert(result.semanticIdentityDurable)
    assert(result.artifactIdentityDurable)

  test("the documented shortest-path feature and provenance workflow executes"):
    val result = AlphaWalkthrough.shortestPath.toOption.get

    assertEquals(result.planHeading, "Shortest-path execution plan")
    assert(result.trainingSimilarity > 0.0)
    assert(result.featureCount > 0)
    assert(result.provenanceEntries > 0)
    assert(result.sharedContributions > 0)
