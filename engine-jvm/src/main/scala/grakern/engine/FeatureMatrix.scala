package grakern.engine

import gale.linalg.LinAlgError
import gale.sparse.{CSR, DuplicatePolicy, Sparse}
import grakern.core.SampleKey
import scala.collection.mutable

final case class SampleIndex(keys: Vector[SampleKey]):
  private lazy val positions: Map[SampleKey, Int] =
    keys.zipWithIndex.foldLeft(Map.empty[SampleKey, Int]) { case (current, (key, index)) =>
      if current.contains(key) then current else current.updated(key, index)
    }

  def size: Int = keys.size

  def indexOf(key: SampleKey): Option[Int] =
    positions.get(key)

/** One immutable sparse row in canonical increasing-column order. */
final case class SparseFeatureRow(
    indices: Vector[Int],
    values: Vector[Double]
):
  require(indices.size == values.size, "sparse indices and values must have equal length")

  def activeSize: Int = indices.size

  def valueAt(column: Int): Double =
    var low = 0
    var high = indices.size - 1
    while low <= high do
      val middle = (low + high) >>> 1
      val candidate = indices(middle)
      if candidate < column then low = middle + 1
      else if candidate > column then high = middle - 1
      else return values(middle)
    0.0

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
):
  private lazy val sparseIndex = SparseCSRIndex(values)

  /** Return one row without allocating a dense vector of `columns.size`. */
  def sparseRow(row: Int): Option[SparseFeatureRow] =
    sparseIndex.row(row)

  private[engine] def postingStart(column: Int): Int = sparseIndex.postingStart(column)
  private[engine] def postingEnd(column: Int): Int = sparseIndex.postingEnd(column)
  private[engine] def postingRow(index: Int): Int = sparseIndex.postingRow(index)
  private[engine] def postingValue(index: Int): Double = sparseIndex.postingValue(index)

/** Row-only sparse view used by query results, whose feature-column dimension can be much larger
  * than their stored row. Unlike [[SparseCSRIndex]], this does not build training postings.
  */
final private[engine] class SparseCSRRows private (
    private val rowEntries: Vector[SparseFeatureRow]
):
  def row(index: Int): Option[SparseFeatureRow] = rowEntries.lift(index)

private[engine] object SparseCSRRows:
  def apply(values: CSR): SparseCSRRows =
    val rowIndices = Array.fill(values.rows)(mutable.ArrayBuffer.empty[Int])
    val rowValues = Array.fill(values.rows)(mutable.ArrayBuffer.empty[Double])
    values.foreachStoredEntry { (row, column, value) =>
      if value != 0.0 then
        rowIndices(row) += column
        rowValues(row) += value
    }
    new SparseCSRRows(
      Vector.tabulate(values.rows)(row =>
        SparseFeatureRow(rowIndices(row).toVector, rowValues(row).toVector)
      )
    )

final private[engine] class SparseCSRIndex private (
    private val source: CSR,
    private val postingOffsets: Array[Int],
    private val postingRows: Array[Int],
    private val postingValues: Array[Double]
):
  private lazy val rowEntries = SparseCSRRows(source)

  def row(index: Int): Option[SparseFeatureRow] = rowEntries.row(index)
  def postingStart(column: Int): Int = postingOffsets(column)
  def postingEnd(column: Int): Int = postingOffsets(column + 1)
  def postingRow(index: Int): Int = postingRows(index)
  def postingValue(index: Int): Double = postingValues(index)

private[engine] object SparseCSRIndex:
  def apply(values: CSR): SparseCSRIndex =
    val columnCounts = new Array[Int](values.cols)
    var nonZeros = 0
    values.foreachStoredEntry { (_, column, value) =>
      if value != 0.0 then
        columnCounts(column) += 1
        nonZeros += 1
    }
    val postingOffsets = new Array[Int](values.cols + 1)
    var column = 0
    while column < columnCounts.length do
      postingOffsets(column + 1) = postingOffsets(column) + columnCounts(column)
      column += 1
    val postingCursors = postingOffsets.clone()
    val postingRows = new Array[Int](nonZeros)
    val postingValues = new Array[Double](nonZeros)
    values.foreachStoredEntry { (row, column, value) =>
      if value != 0.0 then
        val destination = postingCursors(column)
        postingRows(destination) = row
        postingValues(destination) = value
        postingCursors(column) += 1
    }
    new SparseCSRIndex(
      values,
      postingOffsets,
      postingRows,
      postingValues
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
