package org.codefork.aoc2018

import scala.annotation.tailrec

object Day11 {

  val INPUT = 7857

  val MAX_POWER = 4

  case class Square(x: Int, y: Int, squareSize: Int)

  case class SquarePower(x: Int = 0, y: Int = 0, size: Int = 0, power: Int = 0)

  case class Grid(width: Int, height: Int, serialNumber: Int) {

    val grid =
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
      val i = ((rackId * y) + serialNumber) * rackId
      ((i / 100) % 10) - 5
    }

    def getPowerOfSquare(x: Int, y: Int, squareSize: Int) =
      y.to(y + squareSize - 1).map(y_ => x.to(x + squareSize - 1).map(x_ => grid(x_, y_)).sum).sum

    def getPowerOfSquare(square: Square): Int = getPowerOfSquare(square.x, square.y, square.squareSize)

    def getPower3x3(x: Int, y: Int) =
      getPowerOfSquare(x, y, 3)

    /**
     * recursively increase size as long as the max potential power at the next size is higher than
     * the current largest square; otherwise we can short-circuit and stop, saving a lot of calculation time.
     */
    @tailrec
    final def increasingSquares(cur: Square, last: SquarePower, largest: SquarePower): SquarePower = {
      if (cur.x + cur.squareSize - 1 <= width && cur.y + cur.squareSize - 1 <= height) {
        val sumOfEdges = cur.x.to(cur.x + cur.squareSize - 1).map(x_ => grid(x_, cur.y + cur.squareSize - 1)).sum +
          cur.y.to(cur.y + cur.squareSize - 1).map(y_ => grid(cur.x + cur.squareSize - 1, y_)).sum -
          grid(cur.x + cur.squareSize - 1, cur.y + cur.squareSize - 1)
        val newPower = last.power + sumOfEdges
        val newLast = SquarePower(cur.x, cur.y, cur.squareSize, newPower)
        val newLargest = if (newPower > largest.power) newLast else largest
        val potentialPowerOfNextSquare = newPower + (MAX_POWER * (2 * (cur.squareSize + 1) - 1))
        if (potentialPowerOfNextSquare >= newLargest.power) {
          increasingSquares(
            cur = cur.copy(squareSize = cur.squareSize + 1),
            last = newLast,
            largest = newLargest)
        } else {
          //println(s"stopping at size ${cur}")
          largest
        }
      } else {
        largest
      }
    }

    /**
     * calculate power of squares at x,y from size = 1 to largest possible for the coordinate.
     */
    def findSquareWithHighestPower(x: Int, y: Int): SquarePower = {
      val start = Day11.SquarePower(x, x, 1, grid(x, y))
      increasingSquares(cur = Square(x, y, 1), last = start, largest = start)
    }

    def display() = {
      1.to(height).foreach(y => {
        1.to(width).foreach(x =>{
          print(grid(x, y).toString.reverse.padTo(3, ' ').reverse + " ")
        })
        println
      })
    }

  }

}
