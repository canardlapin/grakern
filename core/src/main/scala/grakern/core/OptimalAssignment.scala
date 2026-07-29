package grakern.core

import grakern.core.internal.KernelExpr

object OptimalAssignment:
  def apply[A](base: StrongKernel[A]): Kernel[FiniteBag[A]] =
    Kernel.fromExpression(
      KernelExpr.OptimalAssignment(
        PlanNodeId.fresh(),
        base.expression,
        (left, right) => reference(base, left, right),
        base.assignmentEvaluator
      )
    )

  private[grakern] def reference[A](
      base: Kernel[A],
      left: FiniteBag[A],
      right: FiniteBag[A]
  ): Double =
    Hungarian.maximum(left, right)(base.evaluate)

private[grakern] object Hungarian:
  def maximum[A](
      left: FiniteBag[A],
      right: FiniteBag[A]
  )(score: (A, A) => Double): Double =
    val size = math.max(left.size, right.size)
    if size == 0 then 0.0
    else
      val weights = Array.ofDim[Double](size, size)
      var row = 0
      while row < left.size do
        var column = 0
        while column < right.size do
          weights(row)(column) = score(left.items(row), right.items(column))
          column += 1
        row += 1
      val assignment = minimumAssignment(weights.map(_.map(-_)))
      var result = 0.0
      row = 0
      while row < size do
        result += weights(row)(assignment(row))
        row += 1
      result

  private def minimumAssignment(cost: Array[Array[Double]]): Array[Int] =
    val size = cost.length
    val rowPotential = Array.fill(size + 1)(0.0)
    val columnPotential = Array.fill(size + 1)(0.0)
    val matchedRow = Array.fill(size + 1)(0)
    val previousColumn = Array.fill(size + 1)(0)
    var row = 1

    while row <= size do
      matchedRow(0) = row
      var currentColumn = 0
      val minimum = Array.fill(size + 1)(Double.PositiveInfinity)
      val used = Array.fill(size + 1)(false)
      var augmented = false

      while !augmented do
        used(currentColumn) = true
        val currentRow = matchedRow(currentColumn)
        var delta = Double.PositiveInfinity
        var nextColumn = 0
        var column = 1
        while column <= size do
          if !used(column) then
            val reduced =
              cost(currentRow - 1)(column - 1) -
                rowPotential(currentRow) -
                columnPotential(column)
            if reduced < minimum(column) then
              minimum(column) = reduced
              previousColumn(column) = currentColumn
            if minimum(column) < delta then
              delta = minimum(column)
              nextColumn = column
          column += 1

        column = 0
        while column <= size do
          if used(column) then
            rowPotential(matchedRow(column)) += delta
            columnPotential(column) -= delta
          else minimum(column) -= delta
          column += 1

        currentColumn = nextColumn
        augmented = matchedRow(currentColumn) == 0

      var continue = true
      while continue do
        val prior = previousColumn(currentColumn)
        matchedRow(currentColumn) = matchedRow(prior)
        currentColumn = prior
        continue = currentColumn != 0
      row += 1

    val assignment = Array.fill(size)(0)
    var column = 1
    while column <= size do
      assignment(matchedRow(column) - 1) = column - 1
      column += 1
    assignment
