package org.codefork.aoc2018

import scala.io.Source

object Day18 {

  case class Land(area: Map[XY, Char] = Map.empty, adjacentCache: Map[XY, Seq[XY]] = Map.empty) {
    def maxX = area.keys.maxBy(_.x).x
    def maxY = area.keys.maxBy(_.y).y

    def populateAdjacentCache(): Land = {
      val cache = 0.to(maxY)
        .foldLeft(Map[XY, Seq[XY]]()) { (acc, y) => {
          0.to(maxX).foldLeft(acc) { (acc, x) =>
            acc + (XY(x,y) -> adjacent(XY(x, y)))
          }
       }}
      copy(adjacentCache = cache)
    }

    def adjacent(xy: XY): Seq[XY] = {
      if(adjacentCache.nonEmpty) {
        adjacentCache(xy)
      } else {
        Seq(
          XY(xy.x - 1, xy.y - 1),
          XY(xy.x, xy.y - 1),
          XY(xy.x + 1, xy.y - 1),
          XY(xy.x - 1, xy.y),
          XY(xy.x + 1, xy.y),
          XY(xy.x - 1, xy.y + 1),
          XY(xy.x, xy.y + 1),
          XY(xy.x + 1, xy.y + 1)
        ).filter(xy => xy.x >= 0 && xy.y >= 0 && xy.x <= maxX && xy.y <= maxY)
      }
    }

    def display() = {
      0.to(maxY)
        .foreach(y => {
          val row = 0.to(maxX).foldLeft("") { (acc, x) =>
            acc + area(XY(x, y)).toString
          }
          println(row)
        })
    }

    def transform = {
      copy(area.map {
        case (xy, ch) => {
          val adj = adjacent(xy)
          val newCh = ch match {
            case '.' =>
              if (adj.count(adjXY => area(adjXY) == '|') >= 3) '|' else '.'
            case '|' =>
              if (adj.count(adjXY => area(adjXY) == '#') >= 3) '#' else '|'
            case '#' =>
              if (adj.count(adjXY => area(adjXY) == '#') >= 1 &&
                  adj.count(adjXY => area(adjXY) == '|') >= 1) '#'
              else '.'
          }
          (xy -> newCh)
        }
      })
    }

    def resourceValue = {
      val values = area.values
      val result = values.count(_ == '#') * values.count(_ == '|')
      //println("resourceValue = " + result + " yard count=" + values.count(_ == '#') + " tree count=" + values.count(_ == '|'))
      result
    }
  }

  case class XY(x: Int, y: Int)

  def getInput = {
    val url = getClass.getResource("/day18/input.txt")
    val lines = Source.fromURL(url).getLines().toSeq

    lines.zipWithIndex.foldLeft(Land()) {
      case (acc, (line, y)) => {
        line.zipWithIndex.foldLeft(acc) {
          case (acc, (ch, x)) => {
            acc.copy(area = acc.area + (XY(x, y) -> ch))
          }
        }
      }
    }
  }

}
