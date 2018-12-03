package org.codefork.aoc2018

import scala.io.Source

class Day3 {
  val url = getClass.getResource("/day3/input.txt")
  val claims = Source.fromURL(url).getLines().toSeq.map(Claim.parse(_))
  val overlaps = Claim.findOverlapsAll(Set[(Int, Int)](), claims)
}
