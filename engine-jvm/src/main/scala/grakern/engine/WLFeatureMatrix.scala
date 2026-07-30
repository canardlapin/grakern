package grakern.engine

import gale.linalg.LinAlgError
import grakern.core.SampleKey

enum WLFeatureMetadata:
  case Subtree(
      blockPath: Vector[Int],
      feature: WLSubtreeFeature
  )
  case OptimalAssignment(
      blockPath: Vector[Int],
      feature: grakern.core.HierarchyFeature[WLHierarchyNode]
  )

private[grakern] object WLFeatureMatrix:
  def materialize(
      sampleKeys: Vector[SampleKey],
      rows: Vector[ExactFeatureRow[WLFeatureCoordinate]]
  ): Either[
    LinAlgError,
    FeatureMatrix[WLFeatureCoordinate, WLFeatureMetadata]
  ] =
    ExactFeatureMatrix.materialize(
      sampleKeys,
      rows,
      WLCompiler.sameCoordinate,
      {
        case WLFeatureCoordinate(
              path,
              WLCompiledFeature.Subtree(feature)
            ) =>
          WLFeatureMetadata.Subtree(path, feature)
        case WLFeatureCoordinate(
              path,
              WLCompiledFeature.OptimalAssignment(feature)
            ) =>
          WLFeatureMetadata.OptimalAssignment(
            path,
            feature
          )
      }
    )
