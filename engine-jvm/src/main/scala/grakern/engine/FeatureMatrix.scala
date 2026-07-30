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

final class FeatureDictionary[Coordinate] private[engine] (
    val coordinates: Vector[Coordinate],
    private val sameCoordinate: (
        Coordinate,
        Coordinate
    ) => Boolean
):
  def size: Int = coordinates.size

  def coordinate(index: Int): Option[Coordinate] =
    coordinates.lift(index)

  def indexOf(coordinate: Coordinate): Option[Int] =
    coordinates.indexWhere(
      sameCoordinate(_, coordinate)
    ) match
      case -1    => None
      case index => Some(index)

final case class FeatureMetadata[+Metadata](
    columns: Vector[Metadata]
)

final case class FeatureMatrix[Coordinate, +Metadata](
    values: CSR,
    rows: SampleIndex,
    columns: FeatureDictionary[Coordinate],
    metadata: FeatureMetadata[Metadata]
)

private[grakern] object ExactFeatureMatrix:
  def materialize[Coordinate, Metadata](
      sampleKeys: Vector[SampleKey],
      rows: Vector[ExactFeatureRow[Coordinate]],
      sameCoordinate: (
          Coordinate,
          Coordinate
      ) => Boolean,
      metadata: Coordinate => Metadata
  ): Either[
    LinAlgError,
    FeatureMatrix[Coordinate, Metadata]
  ] =
    val coordinates =
      rows.iterator
        .flatMap(_.entries.iterator.map(_._1))
        .foldLeft(Vector.empty[Coordinate]) { (current, coordinate) =>
          if current.exists(
              sameCoordinate(_, coordinate)
            )
          then current
          else current :+ coordinate
        }
    val dictionary =
      new FeatureDictionary(coordinates, sameCoordinate)
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
            coordinates.map(metadata)
          )
        )
      }
