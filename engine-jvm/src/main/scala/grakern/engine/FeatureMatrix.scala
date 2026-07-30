package grakern.engine

import gale.linalg.LinAlgError
import gale.sparse.{CSR, DuplicatePolicy, Sparse}
import grakern.core.SampleKey
import scala.collection.mutable

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
  private val hashes =
    new Array[Int](coordinates.size)
  private val table =
    Array.fill(FeatureDictionary.tableSize(coordinates.size))(-1)
  private val mask =
    table.length - 1
  private var buildIndex = 0
  while buildIndex < coordinates.size do
    val hash =
      coordinateHash(coordinates(buildIndex))
    hashes(buildIndex) = hash
    var slot =
      FeatureDictionary.spread(hash) & mask
    while table(slot) >= 0 do slot = (slot + 1) & mask
    table(slot) = buildIndex
    buildIndex += 1

  def size: Int = coordinates.size

  def coordinate(index: Int): Option[Coordinate] =
    coordinates.lift(index)

  def indexOf(coordinate: Coordinate): Option[Int] =
    indexOrNegative(coordinate) match
      case index if index >= 0 => Some(index)
      case _                   => None

  private[engine] def indexOrNegative(
      coordinate: Coordinate
  ): Int =
    val hash =
      coordinateHash(coordinate)
    var slot =
      FeatureDictionary.spread(hash) & mask
    var candidate =
      table(slot)
    while candidate >= 0 do
      if hashes(candidate) == hash &&
        sameCoordinate(
          coordinates(candidate),
          coordinate
        )
      then return candidate
      slot = (slot + 1) & mask
      candidate = table(slot)
    -1

private object FeatureDictionary:
  def tableSize(entries: Int): Int =
    val target =
      math.max(2L, entries.toLong * 2L)
    var size = 2
    while size.toLong < target do size = size << 1
    size

  def spread(hash: Int): Int =
    hash ^ (hash >>> 16)

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
    val coordinateInterner =
      new ExactCoordinateInterner(
        coordinateHash,
        sameCoordinate
      )
    val columnRows =
      new Array[Array[Int]](rows.size)
    var rowIndex = 0
    while rowIndex < rows.size do
      val entries = rows(rowIndex).entries
      val columns = new Array[Int](entries.size)
      var entryIndex = 0
      while entryIndex < entries.size do
        columns(entryIndex) = coordinateInterner.intern(
          entries(entryIndex)._1
        )
        entryIndex += 1
      columnRows(rowIndex) = columns
      rowIndex += 1
    val coordinates =
      coordinateInterner.coordinates
    val dictionary =
      new FeatureDictionary(
        coordinates,
        coordinateHash,
        sameCoordinate
      )
    val builder = Sparse.coo(rows.size, dictionary.size)
    rowIndex = 0
    while rowIndex < rows.size do
      val row = rows(rowIndex)
      var entryIndex = 0
      while entryIndex < row.entries.size do
        val entry = row.entries(entryIndex)
        builder.add(
          rowIndex,
          columnRows(rowIndex)(entryIndex),
          entry._2
        ): Unit
        entryIndex += 1
      rowIndex += 1
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

  final private class ExactCoordinateInterner[Coordinate](
      coordinateHash: Coordinate => Int,
      sameCoordinate: (
          Coordinate,
          Coordinate
      ) => Boolean
  ):
    private val observed =
      mutable.ArrayBuffer.empty[Coordinate]
    private val hashes =
      mutable.ArrayBuffer.empty[Int]
    private var table =
      Array.fill(16)(-1)

    def coordinates: Vector[Coordinate] =
      observed.toVector

    def intern(coordinate: Coordinate): Int =
      val hash = coordinateHash(coordinate)
      val existing =
        find(coordinate, hash)
      if existing >= 0 then existing
      else
        ensureCapacity()
        val index = observed.size
        observed += coordinate
        hashes += hash
        insert(index, hash)
        index

    private def find(
        coordinate: Coordinate,
        hash: Int
    ): Int =
      val mask = table.length - 1
      var slot =
        FeatureDictionary.spread(hash) & mask
      var candidate =
        table(slot)
      while candidate >= 0 do
        if hashes(candidate) == hash &&
          sameCoordinate(
            observed(candidate),
            coordinate
          )
        then return candidate
        slot = (slot + 1) & mask
        candidate = table(slot)
      -1

    private def ensureCapacity(): Unit =
      if (observed.size + 1) * 2 > table.length then
        val previousSize = observed.size
        table = Array.fill(table.length << 1)(-1)
        var index = 0
        while index < previousSize do
          insert(index, hashes(index))
          index += 1

    private def insert(index: Int, hash: Int): Unit =
      val mask = table.length - 1
      var slot =
        FeatureDictionary.spread(hash) & mask
      while table(slot) >= 0 do slot = (slot + 1) & mask
      table(slot) = index
