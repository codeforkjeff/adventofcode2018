package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day25 {

  type Point = Seq[Int]

  val origin = Seq(0, 0, 0, 0)

  case class Constellation(points: Seq[Point], maxDistFromOrigin: Int) {

    def add(point: Point) =
      copy(points = points :+ point, maxDistFromOrigin = Seq(dist(point, origin), maxDistFromOrigin).max)

    def belongs(point: Point) =
      points.exists(p => dist(p, point) <= 3)

    def possiblyBelongs(point: Point) =
      val distFromOrigin = dist(point, origin)
      math.abs(maxDistFromOrigin - distFromOrigin) <= 3

    def merge(c: Constellation) =
      copy(points = c.points ++ points, maxDistFromOrigin = Seq(maxDistFromOrigin, c.maxDistFromOrigin).max)
  }

  object Constellation {
    // constructor for Constellation containing a single point
    def apply(point: Point): Constellation =
      Constellation(Seq(point), dist(point, origin))
  }

  def dist(p1: Point, p2: Point) =
    0.until(p1.size).map(dim => math.abs(p1(dim) - p2(dim))).sum

  @tailrec
  def gropupConstellations(sorted: Seq[Point], constellations: Seq[Constellation] = Seq.empty) : Seq[Constellation] = {
    if (sorted.nonEmpty) {
      val point = sorted.head

      val remainder = sorted.tail
      if (constellations.nonEmpty) {

        val constellationsToTest = constellations.filter(c => c.possiblyBelongs(point))

        val constellationsToJoin = constellationsToTest.filter(c => c.belongs(point))

        val toAdd = if (constellationsToJoin.nonEmpty) {
          constellationsToJoin.reduce((acc, c) => acc.merge(c)).add(point)
        } else {
          Constellation(point)
        }

        val newConstellations = constellations.filterNot(constellationsToJoin.contains(_)) :+ toAdd

        gropupConstellations(remainder, newConstellations)
      } else {
        gropupConstellations(remainder, Seq(Constellation(point)))
      }
    } else {
      constellations
    }
  }

  def countConstellations(points: Seq[Point]): Int = {
    val sorted = points.sortBy(dist(_, origin))
    gropupConstellations(sorted).size
  }

  def getInput(path: String) = {
    val url = getClass.getResource(path)
    val s = Source.fromURL(url)
    s.getLines().map(line =>
      line.split(",").map(_.toInt).toSeq
    ).toSeq
  }

}
