package org.codefork.aoc2018

import org.codefork.aoc2018.Day11.Square

import scala.annotation.tailrec

object Day11Part2 extends Part {

  override def status = Finished

  override def answer: String = {
    val width = 300
    val height = 300
    val grid = Day11.Grid(width, height, Day11.INPUT)

    val coords = 1.to(300).flatMap { y =>
      1.to(300).map { x => (x, y) }
    }

    // for this to work, we have to find the largest square for each coord BEFORE comparing
    // to the largest so far in the grid as a whole
    val largest = coords.foldLeft(Day11.SquarePower(1, 1, 1, grid.grid(1,1))) { (largest, xy) => {
      val largestForCoord = grid.findSquareWithHighestPower(xy._1, xy._2)
      if(largestForCoord.power > largest.power) largestForCoord else largest
    }}

    (largest.x, largest.y, largest.size).toString()
  }

}
