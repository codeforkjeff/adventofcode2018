package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day22 {

  case class XY(x: Int, y: Int) {
    def adjacent(maxBound: Int): Set[XY] = {
      Set(
        XY(this.x - 1, this.y),
        XY(this.x + 1, this.y),
        XY(this.x, this.y - 1),
        XY(this.x, this.y + 1)
      ).filter(xy => xy.x >= 0 && xy.y >= 0 && xy.x <= maxBound && xy.y <= maxBound)
    }
  }

  sealed trait Tool
  object ClimbingGear extends Tool
  object Torch extends Tool
  object Neither extends Tool

  // vertices in the graph for shortest path are defined a tuple of (xy, tool), not just xy.
  case class Move(xy: XY, tool: Tool)

  case class PQEntry(move: Move, distance: Long)

  object PQEntryOrder extends Ordering[PQEntry] {
    def compare(x: PQEntry, y: PQEntry): Int =
      y.distance compare x.distance
  }

  case class CaveMap(depth: Int, target: XY) {

    val maxBound = Math.max(target.x, target.y) * 2

    val erosionLevels: Map[XY, Long] = calculateErosionLevels()

    @tailrec
    final def calculateErosionLevels(
        xy: XY = XY(0, 0),
        acc: Map[XY, Long] = Map.empty[XY, Long]): Map[XY, Long] = {
      val geologicLevel: Long =
        if ((xy.x == 0 && xy.y == 0) ||
            (xy.x == target.x && xy.y == target.y)) {
          0
        } else if (xy.y == 0) {
          xy.x * 16807
        } else if (xy.x == 0) {
          xy.y * 48271
        } else {
          acc(XY(xy.x - 1, xy.y)) * acc(XY(xy.x, xy.y - 1))
        }
      val erosion = (geologicLevel + depth) % 20183
      val newAcc = acc + (XY(xy.x, xy.y) -> erosion)

      // calculate for a large bounding box
      val newXYOpt: Option[XY] = if (xy.x == maxBound) {
        if (xy.y == maxBound) {
          None
        } else {
          Some(XY(0, xy.y + 1))
        }
      } else {
        Some(XY(xy.x + 1, xy.y))
      }
      if (newXYOpt.isDefined) {
        calculateErosionLevels(newXYOpt.get, newAcc)
      } else {
        newAcc
      }
    }

    // calculate the erosion level for XY on demand
    /*
    def getErosionLevel(xy: XY): Long = {
      val geologicLevel =
        if ((xy.x == 0 && xy.y == 0) ||
          (xy.x == target.x && xy.y == target.y)) {
          0
        } else if (xy.y == 0) {
          xy.x * 16807
        } else if (xy.x == 0) {
          xy.y * 48271
        } else {
          getErosionLevel(XY(xy.x - 1, xy.y)) * getErosionLevel(XY(xy.x, xy.y - 1))
        }
      val erosion = (geologicLevel + depth) % 20183
      return erosion
    }
    */

    def calculateRisk: Long = {
      0.to(target.x)
        .foldLeft(0L)((acc, x) => {
          val sum = 0
            .to(target.y)
            .foldLeft(0L)((acc, y) => {
              acc + (erosionLevels(XY(x, y)) % 3)
            })
          acc + sum
        })
    }

    // adaptation of Dijkstra's algorithm.
    // there's no immutable PriorityQueue or min heap so we use the mutable one. ugh.
    @tailrec
    final def shortestPath(visited: Set[Move] = Set.empty[Move],
                           pq: mutable.PriorityQueue[PQEntry] = mutable.PriorityQueue[PQEntry](
                             PQEntry(Move(XY(0,0), Torch), 0L))(PQEntryOrder),
                           dist: Map[Move, Long] = Map((Move(XY(0,0), Torch) -> 0L)),
                           step: Int = 0): Long = {

      var minMove = pq.dequeue().move
      // so if we've visited a move already ((because it had a shorter dist),
      // we can ignore the 2nd one with longer dist. this is to make up for
      // being unable to change pri of already queued items
      while(visited.contains(minMove)) {
        minMove = pq.dequeue().move
      }

      val minXY = minMove.xy
      val minDist = dist(minMove)
      val minTool = minMove.tool
      val minRegion = erosionLevels(minXY) % 3

      // println(minXY + " - " + minTool + " - " + minDist + " step " + step)

      if(minXY == target) {
        return minDist + (if(minTool != Torch) 7 else 0)
      }

      val newVisited = visited + minMove

      val neighbors = minXY.adjacent(maxBound)

      val neighborDist = neighbors.map(xy => {

        val neighborRegionType = erosionLevels(xy) % 3

        val weight =
          if(neighborRegionType == 0) {
            if (minTool == ClimbingGear || minTool == Torch) 1 else 8
          } else if (neighborRegionType == 1) {
            if(minTool == ClimbingGear || minTool == Neither) 1 else 8
          } else if (neighborRegionType == 2) {
            if(minTool == Torch || minTool == Neither) 1 else 8
          } else {
            throw new Exception("argh")
          }

        val newTool =
          if(minRegion != neighborRegionType) {
            if(minRegion == 0 && neighborRegionType == 1 && minTool == Torch) {
              ClimbingGear
            } else if(minRegion == 0 && neighborRegionType == 2 && minTool == ClimbingGear) {
              Torch
            } else if(minRegion == 1 && neighborRegionType == 0 && minTool == Neither) {
              ClimbingGear
            } else if(minRegion == 1 && neighborRegionType == 2 && minTool == ClimbingGear) {
              Neither
            } else if(minRegion == 2 && neighborRegionType == 0 && minTool == Neither) {
              Torch
            } else if(minRegion == 2 && neighborRegionType == 1 && minTool == Torch) {
              Neither
            } else {
              minTool
            }
          } else {
            minTool
          }

        val old = dist.getOrElse(Move(xy, newTool), Long.MaxValue)
        val alt = minDist + weight

        if(alt < old) {
          Some((Move(xy, newTool) -> alt))
        } else {
          None
        }
      }).filter(_.isDefined).map(_.get)

      val newDist = dist ++ neighborDist

      // this will queue already-queued Moves, but with a higher priority,
      // which is okay b/c we filter them when dequeueing
      neighborDist.map(item => PQEntry(item._1, item._2))
        .foreach(entry => {
          // println("queuing " + entry.move + " dist=" + entry.distance)
          pq.enqueue(entry)
        })

      shortestPath(newVisited, pq, newDist, step + 1)
    }

    def output: String = {
      0.to(target.y)
        .foldLeft("")((acc, y) => {
          val line = 0
            .to(target.x)
            .foldLeft("")((acc, x) => {
              val risk = (erosionLevels(XY(x, y)) % 3)
              val ch = if (risk == 0L) {
                "."
              } else if (risk == 1L) {
                "="
              } else if (risk == 2L) {
                "|"
              } else {
                "X"
              }
              acc + ch
            })
          acc + line + "\n"
        })
    }

  }

  def buildCaveMapFromInput(): CaveMap = {
    val url = getClass.getResource("/day22/input.txt")
    val lines = Source.fromURL(url).getLines().toList

    val depthRe = raw"depth: (\d+)".r
    val targetRe = raw"target: (\d+),(\d+)".r
    val depth = (lines(0) match { case depthRe(depth) => depth }).toInt
    val coords = (lines(1) match { case targetRe(x, y) => List(x, y) }).map(_.toInt)
    val target = XY(coords(0), coords(1))

    Day22.CaveMap(depth, target)
  }

}
