package org.codefork.aoc2018

import scala.io.Source

object Day23 {

  case class XYZ(x: Long, y: Long, z: Long) {

    def distanceTo(o: XYZ): Long =
      Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)

    // returns all the coordinates of a box around a point, where corners
    // are a distance d from coordinate along each axis
    def containingBox(d: Long): Seq[XYZ] =
      (z - d).to(z).flatMap(z =>
        (y - d).to(y).flatMap(y =>
          (x - d).to(x).map(x => XYZ(x, y, z))
        )
      )

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
