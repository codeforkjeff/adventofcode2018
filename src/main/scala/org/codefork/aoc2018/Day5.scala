package org.codefork.aoc2018

import scala.io.Source

object Day5 {

  def getInputPolymer: String = {
    val url = getClass.getResource("/day5/input.txt")
    Source.fromURL(url).mkString.stripLineEnd
  }

  def reactPolymer(s: String, pos: Int = 0): String = {
    if (pos == s.length - 1) {
      s
    } else {
      val first = s(pos)
      val second = s(pos + 1)
      if (first != second && first.toUpper == second.toUpper) {
        // val reduced = s.patch(pos, "", 2)
        // this is way faster than patch()
        val reduced = s.substring(0, pos) + s.substring(pos + 2)
        val newPos = if (pos - 1 < 0) 0 else pos - 1
        reactPolymer(reduced, newPos)
      } else {
        reactPolymer(s, pos + 1)
      }
    }
  }

  // returns all uppercase
  def getDistinctUnits(s: String): String =
    s.toUpperCase()
      .foldLeft(Set[String]()) { (acc, letter) =>
        acc + letter.toString
      }
      .mkString

}
