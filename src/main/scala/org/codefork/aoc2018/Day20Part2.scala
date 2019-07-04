package org.codefork.aoc2018

import scala.io.Source

object Day20Part2 extends Part {

  override def answer: String = {
    val url = getClass.getResource("/day20/input.txt")
    val re = Source.fromURL(url).mkString
    return Day20.buildElfMap(re).countShortestDistancesAbove(1000).toString
  }

}
