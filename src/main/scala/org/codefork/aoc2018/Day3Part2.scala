package org.codefork.aoc2018

object Day3Part2 extends Part {

  def main(args: Array[String]): Unit = println(answer)

  override def answer: String = {
    val data = new Day3()
    val nonOverlappingClaim = Day3.Claim.findNonOverlapping(data.overlaps, data.claims)
    nonOverlappingClaim.get.id.toString.replace("#", "")
  }
}
