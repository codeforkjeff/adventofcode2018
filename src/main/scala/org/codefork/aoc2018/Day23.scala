package org.codefork.aoc2018

import scala.io.Source

object Day23 {

  case class XYZ(x: Long, y: Long, z: Long) {

    def distanceTo(o: XYZ): Long =
      Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)

  }

  case class Line(pt1: XYZ, pt2: XYZ) {
//    def points = {
//      if(pt1.x == pt2.x) {
//        pt1.x.to
//      }
//    }
  }

  case class Bot(pos: XYZ, r: Long) {

    def distanceTo(bot2: Bot): Long =
      pos.distanceTo(bot2.pos)

    def withinRange(o: XYZ) = {
      //println("distance from " + pos + " to " + o + " = " + pos.distanceTo(o))
      pos.distanceTo(o) <= r
    }

    def corners: Seq[XYZ] =
      Seq(
        pos.copy(x = pos.x - r),
        pos.copy(x = pos.x + r),
        pos.copy(y = pos.y - r),
        pos.copy(y = pos.y + r),
        pos.copy(z = pos.z - r),
        pos.copy(z = pos.z + r)
      )

    def edges: Seq[Line] =
      Seq(
        Line(pos.copy(x = pos.x - r), pos.copy(y = pos.y - r)),
        Line(pos.copy(x = pos.x - r), pos.copy(y = pos.y + r)),
        Line(pos.copy(x = pos.x - r), pos.copy(z = pos.z - r)),
        Line(pos.copy(x = pos.x - r), pos.copy(z = pos.z + r)),

        Line(pos.copy(y = pos.y - r), pos.copy(x = pos.x + r)),
        Line(pos.copy(y = pos.y - r), pos.copy(z = pos.z - r)),
        Line(pos.copy(y = pos.y - r), pos.copy(z = pos.z + r)),

        Line(pos.copy(y = pos.y + r), pos.copy(x = pos.x + r)),
        Line(pos.copy(y = pos.y + r) ,pos.copy(z = pos.z - r)),
        Line(pos.copy(y = pos.y + r) ,pos.copy(z = pos.z + r)),

        Line(pos.copy(z = pos.z - r), pos.copy(x = pos.x + r)),
        Line(pos.copy(z = pos.z + r), pos.copy(x = pos.x + r))
      )

//    def allEdgePoints =
//      edges.map(edge => {
//
//      })


  }

  val lineRe = raw"pos=<([0-9-]+),([0-9-]+),([0-9-]+)>, r=([0-9-]+)".r

  def getInput: Seq[Bot] = {
    val url = getClass.getResource("/day23/input.txt")
    Source.fromURL(url).getLines().map(line => {
      line match { case lineRe(x,y,z,r) =>
        Bot(XYZ(x.toLong, y.toLong, z.toLong), r.toLong)
      }
    }).toSeq
  }

}
