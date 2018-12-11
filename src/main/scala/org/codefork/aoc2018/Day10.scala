package org.codefork.aoc2018

import scala.io.Source

object Day10 {

  case class CoordSet(coords: Seq[Coord], t: Int = 0) {

    val sortedX = coords.sortBy(_.x)
    val minX = sortedX.head.x
    val maxX = sortedX.last.x

    val sortedY = coords.sortBy(_.y)
    val minY = sortedY.head.y
    val maxY = sortedY.last.y

    val width = maxX - minX
    val height = maxY - minY

    val set = coords.map(c => (c.x, c.y))

    def nextSecond =
      copy(coords = coords.map(_.move), t+1)
  }

  def findLetters(coordSet: CoordSet, lastSetOpt: Option[CoordSet] = None): (String, CoordSet) = {
    // height initially shrinks; when it starts growing again, the last coordset is the answer
    if (lastSetOpt.isDefined && coordSet.height > lastSetOpt.get.height) {
      val lastSet = lastSetOpt.get

      val text = lastSet.minY.to(lastSet.maxY).foldLeft("") { (grid, y) =>
        grid + lastSet.minX.to(lastSet.maxX).foldLeft("") { (line, x) =>
          line + (if (lastSet.set.contains((x, y))) "#" else ".")
        } + "\n"
      }
      (text, lastSet)
    } else {
      findLetters(coordSet.nextSecond, Some(coordSet))
    }
  }


  case class Coord(x: Int, y: Int, vx: Int, vy: Int) {
    def atSecond(sec: Int) = {
      copy(x = x + (vx * sec), y = y + (vy * sec))
    }

    def move = atSecond(1)
  }

  def getCoords = {
    val url = getClass.getResource("/day10/input.txt")
    val coords = Source
      .fromURL(url)
      .getLines()
      .toList
      .map(line => {
        val matches =
          raw"<(.+?)>".r.findAllMatchIn(line).map(m => m.group(1)).toList
        val coords = matches(0).split(",").map(_.replace(" ", "").toInt).toList
        val velocities =
          matches(1).split(",").map(_.replace(" ", "").toInt).toList
        Coord(coords(0), coords(1), velocities(0), velocities(1))
      })
    coords
  }

}
