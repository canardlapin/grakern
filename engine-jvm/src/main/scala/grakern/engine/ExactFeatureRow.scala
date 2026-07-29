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
    entries.iterator.map { case (key, value) =>
      that.entries
        .collectFirst {
          case (candidate, amount) if sameKey(key, candidate) =>
            value * amount
        }
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
