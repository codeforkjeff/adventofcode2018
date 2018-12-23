package org.codefork.aoc2018

object Day17Part1 extends Part {

  override def answer: String = {
    val result = Day17.startFlow(Day17.buildScan)
    //result.display()
    result.countWater.toString
  }

}
