package org.codefork.aoc2018

import scala.io.Source

object Day6 {

  case class Coord(x: Int,
                   y: Int,
                   prevClosestCount: Int = 0,
                   closestCount: Int = 0) {

    // return all coords at a given dist from this coord
    def findAtDist(dist: Int): Set[Coord] = {
      0.to(dist)
        .flatMap(d =>
          if (d == dist) {
            Seq(Coord(x - d, y), Coord(x + d, y))
          } else {
            Seq(Coord(x - d, y + (dist - d)),
                Coord(x - d, y - (dist - d)),
                Coord(x + d, y + (dist - d)),
                Coord(x + d, y - (dist - d)))
        })
        .toSet
    }

    def distTo(c: Coord) = Math.abs(this.x - c.x) + Math.abs(this.y - c.y)

    def area = closestCount + 1
  }

  def getCoords = {
    val url = getClass.getResource("/day6/input.txt")
    Source
      .fromURL(url)
      .getLines()
      .map(_.split(", "))
      .map(a => Coord(a(0).toInt, a(1).toInt))
      .toSeq
  }

}
