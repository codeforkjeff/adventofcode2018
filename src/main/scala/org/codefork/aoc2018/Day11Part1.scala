package org.codefork.aoc2018

object Day11Part1 extends Part {

  override def answer: String = {
    val width = 300
    val height = 300
    val grid = Day11.Grid(width, height, Day11.INPUT)
    val squares = 1.to(height - 2).foldLeft(Map[(Int, Int), Int]()) {
      (acc, y) =>
        1.to(width - 2).foldLeft(acc) { (acc, x) =>
          acc + ((x, y) -> grid.getPower3x3(x, y))
        }
    }
    squares.maxBy(_._2)._1.toString
  }

}
