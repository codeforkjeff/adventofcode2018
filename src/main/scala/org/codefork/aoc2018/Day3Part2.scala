package org.codefork.aoc2018

object Day3Part2 {

  def main(args: Array[String]): Unit = {
    val data = new Day3()
    val nonOverlappingClaim = Day3.Claim.findNonOverlapping(data.overlaps, data.claims)
    println(nonOverlappingClaim.get.id)
  }

}
