package org.codefork.aoc2018

import scala.io.Source

object Day1Part1 extends Part {

  override def answer: String = {
    val url = getClass.getResource("/day1/input.txt")
    val s = Source.fromURL(url)
    val result = s.getLines()
      .foldLeft(0) { (acc, i) => acc + i.toInt }
    result.toString
  }
}
