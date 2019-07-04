package org.codefork.aoc2018

import scala.io.Source

object Day20Part1 extends Part {

  override def answer: String = {
    assert(Day20.findFarthestDistance("^WNE$")== 3)
    assert(Day20.findFarthestDistance("^ENWWW(NEEE|SSE(EE|N))$") == 10)
    assert(Day20.findFarthestDistance("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$") == 18)

    val url = getClass.getResource("/day20/input.txt")
    val re = Source.fromURL(url).mkString
    return Day20.findFarthestDistance(re).toString
  }

}
