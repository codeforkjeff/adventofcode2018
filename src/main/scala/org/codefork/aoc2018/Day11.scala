package org.codefork.aoc2018

object Day11 {

  def createGrid(width: Int, height: Int) =
    1.to(height).foldLeft(Map[(Int, Int), Int]()) { (acc, y) =>
      1.to(width).foldLeft(acc) { (acc, x) =>
        {
          acc + ((x, y) -> getPower(x, y))
        }
      }
    }

  // possible values are -5 through 4
  def getPower(x: Int, y: Int) = {
    val rackId = x + 10
    val i = ((rackId * y) + 7857) * rackId
    ((i / 100) % 10) - 5
  }

  def getPowerOfSquare(grid: Map[(Int, Int), Int], x: Int, y: Int, squareSize: Int) =
    y.to(y + squareSize - 1).map(y_ => x.to(x + squareSize - 1).map(x_ => grid(x_, y_)).sum).sum

  def getPower3x3(grid: Map[(Int, Int), Int], x: Int, y: Int) =
    getPowerOfSquare(grid, x, y, 3)

}
