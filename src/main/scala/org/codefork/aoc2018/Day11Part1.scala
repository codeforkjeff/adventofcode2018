package org.codefork.aoc2018

object Day11Part1 extends Part {

  override def answer: String = {
    val width = 300
    val height = 300
    val grid = Day11.createGrid(width, height)
    val squares = 1.to(height - 2).foldLeft(Map[(Int, Int), Int]()) {
      (acc, y) =>
        1.to(width - 2).foldLeft(acc) { (acc, x) =>
          acc + ((x, y) -> Day11.getPower3x3(grid, x, y))
        }
    }
    squares.maxBy(_._2)._1.toString
  }

}
