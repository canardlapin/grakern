package grakern.engine

import gale.linalg.LinAlgError
import gale.sparse.{CSR, DuplicatePolicy, Sparse}
import grakern.core.SampleKey

final case class SampleIndex(keys: Vector[SampleKey]):
  def size: Int = keys.size

  def indexOf(key: SampleKey): Option[Int] =
    keys.indexOf(key) match
      case -1    => None
      case index => Some(index)

final class FeatureDictionary private[engine] (
    val coordinates: Vector[WLFeatureCoordinate]
):
  def size: Int = coordinates.size

  def coordinate(index: Int): Option[WLFeatureCoordinate] =
    coordinates.lift(index)

  def indexOf(coordinate: WLFeatureCoordinate): Option[Int] =
    coordinates.indexWhere(
      WLCompiler.sameCoordinate(_, coordinate)
    ) match
      case -1    => None
      case index => Some(index)

enum WLFeatureMetadata:
  case Subtree(
      blockPath: Vector[Int],
      feature: WLSubtreeFeature
  )
  case OptimalAssignment(
      blockPath: Vector[Int],
      feature: grakern.core.HierarchyFeature[WLHierarchyNode]
  )

final case class FeatureMetadata(
    columns: Vector[WLFeatureMetadata]
)

final case class FeatureMatrix(
    values: CSR,
    rows: SampleIndex,
    columns: FeatureDictionary,
    metadata: FeatureMetadata
)

private[grakern] object WLFeatureMatrix:
  def materialize(
      sampleKeys: Vector[SampleKey],
      rows: Vector[ExactFeatureRow[WLFeatureCoordinate]]
  ): Either[LinAlgError, FeatureMatrix] =
    val coordinates =
      rows.iterator
        .flatMap(_.entries.iterator.map(_._1))
        .foldLeft(Vector.empty[WLFeatureCoordinate]) { (current, coordinate) =>
          if current.exists(
              WLCompiler.sameCoordinate(_, coordinate)
            )
          then current
          else current :+ coordinate
        }
    val dictionary = new FeatureDictionary(coordinates)
    val builder = Sparse.coo(rows.size, dictionary.size)
    rows.zipWithIndex.foreach { case (row, rowIndex) =>
      row.entries.foreach { case (coordinate, value) =>
        dictionary.indexOf(coordinate).foreach { columnIndex =>
          builder.add(rowIndex, columnIndex, value)
        }
      }
    }
    builder
      .tryToCSR(DuplicatePolicy.Error)
      .map { values =>
        FeatureMatrix(
          values,
          SampleIndex(sampleKeys),
          dictionary,
          FeatureMetadata(
            coordinates.map {
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
        )
      }
