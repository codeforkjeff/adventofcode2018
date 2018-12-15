package org.codefork.aoc2018

object Day11 {

  val INPUT = 7857

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

    def getPower3x3(x: Int, y: Int) =
      getPowerOfSquare(x, y, 3)

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
