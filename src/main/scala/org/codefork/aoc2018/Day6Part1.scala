package org.codefork.aoc2018

import scala.io.Source

object Day6Part1 extends Part {

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

    def area = closestCount + 1
  }

  /**
    * iterate, increasing dist, keeping track of finite areas (the ones that stop growing)
    * as we go, until we hit a ceiling
    *
    * @param dist distance to calcalate coords for
    * @param max ceiling to stop iterating
    * @param coords coordinates to process (reduced with each iter)
    * @param covered coordinates already covered by previous iterations
    * @param withFiniteAreas tally of finite areas, in desc order
    * @return
    */
  def processDist(dist: Int,
                  max: Int,
                  coords: Seq[Coord],
                  covered: Set[Coord] = Set.empty,
                  withFiniteAreas: List[Coord] = List.empty): Coord = {
    if (dist == max) {
      return withFiniteAreas.head
    }

    //println(s"dist=$dist, max=$max")

    // find all the coordinates at a given dist, for each coord in our seq
    val atDist = coords.map(c => c -> c.findAtDist(dist)).toMap

    // find overlaps among coords found at this dist
    val overlapping = atDist.values.flatten
      .groupBy(identity)
      .filter { case (k, v) => v.size > 1 }
      .keys
      .toSet

    //println("# overlapping = " + overlapping.size.toString)

    val coveredAndOverlapping = covered ++ overlapping

    // exclude already covered coordinates and new overlapping ones from new area
    // found at current dist, and update our coords
    val newCoords = atDist
      .map { case (k, v) => k -> v.diff(coveredAndOverlapping) }
      .map {
        case (k, v) =>
          k.copy(prevClosestCount = k.closestCount,
                 closestCount = k.closestCount + v.size)
      }
      .toSeq

    val newFinite = newCoords
      .filter(c => c.prevClosestCount == c.closestCount)
      .sortBy(_.closestCount)
      .reverse
      .toList

    val remaining = newCoords.filter(c => c.prevClosestCount != c.closestCount)

    //println("# coords left=" + remaining.size.toString)

    processDist(dist + 1,
                max,
                remaining,
                covered ++ atDist.values.flatten,
                newFinite ++ withFiniteAreas)
  }

  override def answer: String = {
    val url = getClass.getResource("/day6/input.txt")
    val coords = Source
      .fromURL(url)
      .getLines()
      .map(_.split(", "))
      .map(a => Coord(a(0).toInt, a(1).toInt))
      .toSeq

    // this ceiling is way too high but can't figure out a more reasonable one;
    // dist 86 is last iter we need, but how to determine that?
    val maxX = coords.maxBy(_.x).x - coords.minBy(_.x).x
    val maxY = coords.maxBy(_.y).y - coords.minBy(_.y).y
    val max = Math.max(maxX, maxY)

    processDist(1, max, coords, coords.toSet).area.toString
  }

}
