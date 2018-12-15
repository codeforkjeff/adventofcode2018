package org.codefork.aoc2018

object Day11Part2 extends Part {

  override def status = NeedsOptimization

  override def answer: String = {
    val width = 300
    val height = 300
    val grid = Day11.Grid(width, height, Day11.INPUT)

    // (x, y, squareSize, sum)
    val largest = 1.to(height).foldLeft((0, 0, 0, 0)) { (acc, y) =>
      {
        val largest = 1.to(width).foldLeft(acc) { (acc, x) =>
          {
            val wLimit = width - x + 1
            val hLimit = height - y + 1
            val limit = if (wLimit < hLimit) wLimit else hLimit

            // seed accumulator with 1x1 sq
            val initial = (1, 1, 1, grid.grid(x, y), grid.grid(x, y))

            // (x, y, squareSize, sum, lastSum)
            val largest = 2
              .to(limit)
              .foldLeft(initial) { (acc, squareSize) =>
                {
                  val largestSumSoFar = acc._4
                  val lastSum = acc._5
                  // tack on additional coordinates as square size expands.
                  // this is a bit faster than calculating entire sq each time
                  val newSumY = y
                    .to(y + squareSize - 1)
                    .map(y_ => grid.grid(x + squareSize - 1, y_))
                    .sum
                  // -2 instead of -1 to avoid counting coordinate at bottom right twice
                  val newSumX = x
                    .to(x + squareSize - 2)
                    .map(x_ => grid.grid(x_, y + squareSize - 1))
                    .sum
                  val newSum = lastSum + newSumX + newSumY

                  //println(x, y, squareSize, newSum, newSum)
                  if (newSum > largestSumSoFar) {
                    (x, y, squareSize, newSum, newSum)
                  } else {
                    (acc._1, acc._2, acc._3, acc._4, newSum)
                  }
                }
              }

            if (largest._4 > acc._4) {
              (largest._1, largest._2, largest._3, largest._4)
            } else acc
          }
        }

        if (largest._4 > acc._4) {
          println("new largest=" + largest)
          largest
        } else acc
      }
    }
    (largest._1, largest._2, largest._3).toString()
  }

}
