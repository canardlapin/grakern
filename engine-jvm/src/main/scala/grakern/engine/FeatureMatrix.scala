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
    private val coordinateHash: Coordinate => Int,
    private val sameCoordinate: (
        Coordinate,
        Coordinate
    ) => Boolean
):
  private val buckets =
    coordinates.indices.foldLeft(
      Map.empty[Int, Vector[Int]]
    ) { (current, index) =>
      val hash = coordinateHash(coordinates(index))
      current.updated(
        hash,
        current.getOrElse(hash, Vector.empty) :+ index
      )
    }

  def size: Int = coordinates.size

  def coordinate(index: Int): Option[Coordinate] =
    coordinates.lift(index)

  def indexOf(coordinate: Coordinate): Option[Int] =
    buckets
      .getOrElse(coordinateHash(coordinate), Vector.empty)
      .find(index => sameCoordinate(coordinates(index), coordinate))

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
    materialize(
      sampleKeys,
      rows,
      _ => 0,
      sameCoordinate,
      metadata
    )

  def materialize[Coordinate, Metadata](
      sampleKeys: Vector[SampleKey],
      rows: Vector[ExactFeatureRow[Coordinate]],
      coordinateHash: Coordinate => Int,
      sameCoordinate: (
          Coordinate,
          Coordinate
      ) => Boolean,
      metadata: Coordinate => Metadata
  ): Either[
    LinAlgError,
    FeatureMatrix[Coordinate, Metadata]
  ] =
    val coordinateBuilder =
      Vector.newBuilder[Coordinate]
    val observed =
      scala.collection.mutable.ArrayBuffer.empty[Coordinate]
    val buckets =
      scala.collection.mutable.HashMap.empty[
        Int,
        Vector[Int]
      ]
    rows.iterator
      .flatMap(_.entries.iterator.map(_._1))
      .foreach { coordinate =>
        val hash = coordinateHash(coordinate)
        val candidates =
          buckets.getOrElse(hash, Vector.empty)
        if !candidates.exists(index => sameCoordinate(observed(index), coordinate))
        then
          val index = observed.size
          observed += coordinate
          coordinateBuilder += coordinate
          buckets.update(hash, candidates :+ index)
      }
    val coordinates =
      coordinateBuilder.result()
    val dictionary =
      new FeatureDictionary(
        coordinates,
        coordinateHash,
        sameCoordinate
      )
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
