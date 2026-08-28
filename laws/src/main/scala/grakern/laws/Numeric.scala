package grakern.laws

/** Small, dependency-free numerics for laws that must run on JVM, Scala.js and Scala Native. */
object SymmetricEigen:
  /** Smallest eigenvalue of a symmetric matrix by cyclic Jacobi rotations.
    *
    * Intended for the tiny Gram matrices the generators produce (≤ ~12 rows); accuracy is well
    * below the 1e-9 tolerance the PSD laws use.
    */
  def minEigenvalue(input: Array[Array[Double]]): Double =
    val n = input.length
    if n == 0 then 0.0
    else
      val a = Array.tabulate(n, n)((i, j) => 0.5 * (input(i)(j) + input(j)(i)))
      var sweep = 0
      var off = offDiagonal(a)
      while sweep < 100 && off > 1e-22 do
        var p = 0
        while p < n - 1 do
          var q = p + 1
          while q < n do
            val apq = a(p)(q)
            if math.abs(apq) > 1e-300 then
              val theta = (a(q)(q) - a(p)(p)) / (2.0 * apq)
              val t =
                (if theta >= 0 then 1.0 else -1.0) /
                  (math.abs(theta) + math.sqrt(theta * theta + 1.0))
              val c = 1.0 / math.sqrt(t * t + 1.0)
              val s = t * c
              var k = 0
              while k < n do
                val akp = a(k)(p)
                val akq = a(k)(q)
                a(k)(p) = c * akp - s * akq
                a(k)(q) = s * akp + c * akq
                k += 1
              k = 0
              while k < n do
                val apk = a(p)(k)
                val aqk = a(q)(k)
                a(p)(k) = c * apk - s * aqk
                a(q)(k) = s * apk + c * aqk
                k += 1
            q += 1
          p += 1
        off = offDiagonal(a)
        sweep += 1
      var min = Double.PositiveInfinity
      var i = 0
      while i < n do
        if a(i)(i) < min then min = a(i)(i)
        i += 1
      min

  private def offDiagonal(a: Array[Array[Double]]): Double =
    var total = 0.0
    var i = 0
    while i < a.length do
      var j = 0
      while j < a.length do
        if i != j then total += a(i)(j) * a(i)(j)
        j += 1
      i += 1
    total

object Tolerance:
  val psd: Double = 1e-9
  val algebra: Double = 1e-9
  val bounds: Double = 1e-9

  def close(left: Double, right: Double, tolerance: Double): Boolean =
    val scale = math.max(1.0, math.max(math.abs(left), math.abs(right)))
    math.abs(left - right) <= tolerance * scale
