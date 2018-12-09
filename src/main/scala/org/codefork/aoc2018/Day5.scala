package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day5 {

  def getInputPolymer: String = {
    val url = getClass.getResource("/day5/input.txt")
    Source.fromURL(url).mkString.stripLineEnd
  }

  @tailrec
  def reactPolymerSlow(s: String, pos: Int = 0): String = {
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
        reactPolymerSlow(reduced, newPos)
      } else {
        reactPolymerSlow(s, pos + 1)
      }
    }
  }

  // slow version = ~1100ms for part 1 input; this version using list as stack takes ~60ms
  @tailrec
  def reactPolymer(s: String, reacted: List[Char] = List[Char](), pos: Int = 0): String = {
    if (pos == s.length) {
      reacted.mkString.reverse
    } else {
      val first = if(reacted.nonEmpty) reacted(0) else ' '
      val second = s(pos)
      val unitsReacted = first != second && first.toUpper == second.toUpper
      val newReacted = if(unitsReacted) reacted.tail else second :: reacted
      reactPolymer(s, newReacted, pos + 1)
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
