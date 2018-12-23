package org.codefork.aoc2018

object Day17Part2 extends Part {

  override def answer: String = {
    val result = Day17.startFlow(Day17.buildScan)
    result.countRetained.toString
  }

}
