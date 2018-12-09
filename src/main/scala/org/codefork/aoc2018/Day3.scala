package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

class Day3 {
  val url = getClass.getResource("/day3/input.txt")
  val claims =
    Source.fromURL(url).getLines().toSeq.map(Day3.Claim.parse(_)).sortBy(_.x)
  val overlaps = Day3.Claim.findOverlapsAll(claims)
}

object Day3 {

  case class Claim(id: String, x: Int, y: Int, width: Int, height: Int)

  object Claim {

    def parse(s: String): Claim = {
      val pieces = s.split(" ")
      val coord = pieces(2).replace(":", "").split(",")
      val dim = pieces(3).split("x")
      Claim(pieces(0),
            coord(0).toInt,
            coord(1).toInt,
            dim(0).toInt,
            dim(1).toInt)
    }

    // find overlaps among all claims; assumes remaining is sorted by x
    @tailrec
    def findOverlapsAll(remaining: Seq[Claim],
                        acc: Set[(Int, Int)] = Set[(Int, Int)]()): Set[(Int, Int)] = {
      val newOverlaps = acc ++ findOverlaps(remaining.head, remaining.tail)
      if (remaining.size > 1) {
        findOverlapsAll(remaining.tail, newOverlaps)
      } else {
        newOverlaps
      }
    }

    // find overlaps between claim and the claims in remaining
    // returns set of coordinates, each representing a sq inch
    @tailrec
    def findOverlaps(
        claim: Claim,
        remaining: Seq[Claim],
        acc: Set[(Int, Int)] = Set[(Int, Int)]()): Set[(Int, Int)] = {
      if (remaining.size > 0) {
        val c1 = claim
        val c2 = remaining.head
        // since input is sorted, we can stop when x no longer overlaps
        if (c2.x <= c1.x + c1.width - 1) {
          val c1xvalues = c1.x.to(c1.x + c1.width - 1).toSet
          val c2xvalues = c2.x.to(c2.x + c2.width - 1).toSet
          val c1yvalues = c1.y.to(c1.y + c1.height - 1).toSet
          val c2yvalues = c2.y.to(c2.y + c2.height - 1).toSet
          val xOverlap = c1xvalues.intersect(c2xvalues)
          val yOverlap = c1yvalues.intersect(c2yvalues)
          val newOverlaps = xOverlap.flatMap(x => yOverlap.map(y => (x, y)))
          findOverlaps(claim, remaining.tail, acc ++ newOverlaps)
        } else {
          acc
        }
      } else {
        acc
      }
    }

    // find claim in claims that does NOT overlap with any points in coordinates
    def findNonOverlapping(coordinates: Set[(Int, Int)],
                           claims: Seq[Claim]): Option[Claim] = {
      if (claims.size > 0) {
        val claim = claims.head
        val firstOverlapForClaim = coordinates.find(
          coord =>
            coord._1 >= claim.x && coord._1 <= claim.x + claim.width - 1 &&
              coord._2 >= claim.y && coord._2 <= claim.y + claim.height - 1)

        if (firstOverlapForClaim.isEmpty) {
          Some(claim)
        } else {
          findNonOverlapping(coordinates, claims.tail)
        }
      } else {
        None
      }
    }

  }

}
