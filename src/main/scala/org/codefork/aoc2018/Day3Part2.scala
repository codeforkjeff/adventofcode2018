package org.codefork.aoc2018

object Day3Part2 extends Part {

  override def answer: String = {
    val data = new Day3()
    val nonOverlappingClaim = Day3.Claim.findNonOverlapping(data.overlaps, data.claims)
    nonOverlappingClaim.get.id.toString.replace("#", "")
  }
}
