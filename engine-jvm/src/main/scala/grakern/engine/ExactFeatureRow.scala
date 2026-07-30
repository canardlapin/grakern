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
    val index =
      ExactFeatureRow.Indexed(that.entries, keyHash)
    entries.iterator.map { case (key, value) =>
      index
        .find(key, sameKey)
        .map(amount => value * amount)
        .getOrElse(0.0)
    }.sum

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
      entries: Vector[(K, Double)],
      buckets: Map[Int, Vector[Int]],
      keyHash: K => Int
  ):
    def find(
        key: K,
        sameKey: (K, K) => Boolean
    ): Option[Double] =
      buckets
        .getOrElse(keyHash(key), Vector.empty)
        .collectFirst {
          case index if sameKey(key, entries(index)._1) =>
            entries(index)._2
        }

    def dot(
        left: ExactFeatureRow[K],
        sameKey: (K, K) => Boolean
    ): Double =
      left.entries.iterator.map { case (key, value) =>
        find(key, sameKey)
          .map(amount => value * amount)
          .getOrElse(0.0)
      }.sum

  object Indexed:
    def apply[K](
        entries: Vector[(K, Double)],
        keyHash: K => Int
    ): Indexed[K] =
      val buckets =
        entries.indices.foldLeft(
          Map.empty[Int, Vector[Int]]
        ) { (current, index) =>
          val hash = keyHash(entries(index)._1)
          current.updated(
            hash,
            current.getOrElse(hash, Vector.empty) :+ index
          )
        }
      new Indexed(entries, buckets, keyHash)
