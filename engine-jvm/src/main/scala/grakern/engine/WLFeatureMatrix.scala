package grakern.engine

import gale.linalg.LinAlgError
import gale.sparse.CSRPattern
import grakern.core.SampleKey
import grakern.standard.wl.PortableSparseFeatureRow

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
      WLCompiler.coordinateHash,
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

  def materializePortable(
      sampleKeys: Vector[SampleKey],
      rows: Vector[PortableSparseFeatureRow],
      coordinates: Vector[WLFeatureCoordinate]
  ): Either[
    LinAlgError,
    FeatureMatrix[WLFeatureCoordinate, WLFeatureMetadata]
  ] =
    val dictionary =
      new FeatureDictionary(
        coordinates,
        WLCompiler.coordinateHash,
        WLCompiler.sameCoordinate
      )
    val rowOffsets = new Array[Int](rows.size + 1)
    var nonZeros = 0
    var rowIndex = 0
    while rowIndex < rows.size do
      nonZeros += rows(rowIndex).indices.size
      rowOffsets(rowIndex + 1) = nonZeros
      rowIndex += 1
    val columnIndices = new Array[Int](nonZeros)
    var storedIndex = 0
    rowIndex = 0
    while rowIndex < rows.size do
      val row = rows(rowIndex)
      var entryIndex = 0
      while entryIndex < row.indices.size do
        columnIndices(storedIndex) = row.indices(entryIndex)
        storedIndex += 1
        entryIndex += 1
      rowIndex += 1
    CSRPattern
      .checked(rows.size, coordinates.size, rowOffsets, columnIndices)
      .map { pattern =>
        val values = pattern.valuesBuilder()
        storedIndex = 0
        rowIndex = 0
        while rowIndex < rows.size do
          val row = rows(rowIndex)
          var entryIndex = 0
          while entryIndex < row.values.size do
            values(storedIndex) = row.values(entryIndex)
            storedIndex += 1
            entryIndex += 1
          rowIndex += 1
        values.result()
      }
      .map { values =>
        FeatureMatrix(
          values,
          SampleIndex(sampleKeys),
          dictionary,
          FeatureMetadata(
            coordinates.map {
              case WLFeatureCoordinate(path, WLCompiledFeature.Subtree(feature)) =>
                WLFeatureMetadata.Subtree(path, feature)
              case WLFeatureCoordinate(
                    path,
                    WLCompiledFeature.OptimalAssignment(feature)
                  ) =>
                WLFeatureMetadata.OptimalAssignment(path, feature)
            }
          )
        )
      }
