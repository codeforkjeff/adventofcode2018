package org.codefork.aoc2018

object Day5Part1 extends Part {

  override def answer: String = {
    val s = Day5.getInputPolymer
    val reduced = Day5.reactPolymer(s)
    reduced.length.toString
  }

}
