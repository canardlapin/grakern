package grakern.engine

final private[grakern] case class ExactFeatureRow[K](
    entries: Vector[(K, Double)]
):
  def activeSize: Int = entries.size

  def squaredNorm: Double =
    entries.iterator.map { case (_, value) => value * value }.sum

  def dot(
      that: ExactFeatureRow[K]
  )(sameKey: (K, K) => Boolean): Double =
    dot(that)(_ => 0, sameKey)

  private[grakern] def dot(
      that: ExactFeatureRow[K]
  )(
      keyHash: K => Int,
      sameKey: (K, K) => Boolean
  ): Double =
    ExactFeatureRow
      .Indexed(that.entries, keyHash)
      .dot(this, sameKey)

  def scaled(factor: Double): ExactFeatureRow[K] =
    ExactFeatureRow(
      entries.map { case (key, value) => key -> (factor * value) }
    )

  def mapKeys[L](f: K => L): ExactFeatureRow[L] =
    ExactFeatureRow(entries.map { case (key, value) => f(key) -> value })

  def ++(that: ExactFeatureRow[K]): ExactFeatureRow[K] =
    ExactFeatureRow(entries ++ that.entries)

private[grakern] object ExactFeatureRow:
  final class Indexed[K] private (
      private[engine] val keys: Array[Any],
      private[engine] val values: Array[Double],
      private[engine] val hashes: Array[Int],
      table: Array[Int],
      keyHash: K => Int
  ):
    def find(
        key: K,
        sameKey: (K, K) => Boolean
    ): Option[Double] =
      findIndex(key, keyHash(key), sameKey) match
        case index if index >= 0 => Some(values(index))
        case _                   => None

    def dot(
        left: ExactFeatureRow[K],
        sameKey: (K, K) => Boolean
    ): Double =
      var index = 0
      var result = 0.0
      while index < left.entries.size do
        val entry = left.entries(index)
        result += entry._2 *
          valueOrZero(
            entry._1,
            keyHash(entry._1),
            sameKey
          )
        index += 1
      result

    def dot(
        left: Indexed[K],
        sameKey: (K, K) => Boolean
    ): Double =
      var index = 0
      var result = 0.0
      while index < left.keys.length do
        result += left.values(index) *
          valueOrZero(
            left.keys(index).asInstanceOf[K],
            left.hashes(index),
            sameKey
          )
        index += 1
      result

    private def valueOrZero(
        key: K,
        hash: Int,
        sameKey: (K, K) => Boolean
    ): Double =
      val index = findIndex(key, hash, sameKey)
      if index < 0 then 0.0 else values(index)

    private def findIndex(
        key: K,
        hash: Int,
        sameKey: (K, K) => Boolean
    ): Int =
      val mask = table.length - 1
      var slot = spread(hash) & mask
      var candidate = table(slot)
      while candidate >= 0 do
        if hashes(candidate) == hash &&
          sameKey(
            key,
            keys(candidate).asInstanceOf[K]
          )
        then return candidate
        slot = (slot + 1) & mask
        candidate = table(slot)
      -1

  object Indexed:
    def apply[K](
        entries: Vector[(K, Double)],
        keyHash: K => Int
    ): Indexed[K] =
      val keys = new Array[Any](entries.size)
      val values = new Array[Double](entries.size)
      val hashes = new Array[Int](entries.size)
      val table =
        Array.fill(tableSize(entries.size))(-1)
      val mask = table.length - 1
      var index = 0
      while index < entries.size do
        val entry = entries(index)
        val hash = keyHash(entry._1)
        keys(index) = entry._1
        values(index) = entry._2
        hashes(index) = hash
        var slot = spread(hash) & mask
        while table(slot) >= 0 do slot = (slot + 1) & mask
        table(slot) = index
        index += 1
      new Indexed(
        keys,
        values,
        hashes,
        table,
        keyHash
      )

    private def tableSize(entries: Int): Int =
      val target =
        math.max(2L, entries.toLong * 2L)
      var size = 2
      while size.toLong < target do size = size << 1
      size

  private def spread(hash: Int): Int =
    hash ^ (hash >>> 16)
