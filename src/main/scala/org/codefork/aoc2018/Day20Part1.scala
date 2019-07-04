package org.codefork.aoc2018

import scala.io.Source

object Day20Part1 extends Part {

  override def answer: String = {
    assert(Day20.buildElfMap("^WNE$").findFarthestDistance == 3)
    assert(Day20.buildElfMap("^ENWWW(NEEE|SSE(EE|N))$").findFarthestDistance == 10)
    assert(Day20.buildElfMap("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$").findFarthestDistance == 18)

    val url = getClass.getResource("/day20/input.txt")
    val re = Source.fromURL(url).mkString
    return Day20.buildElfMap(re).findFarthestDistance.toString
  }

}
