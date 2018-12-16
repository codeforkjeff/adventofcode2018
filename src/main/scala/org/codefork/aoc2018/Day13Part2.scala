package org.codefork.aoc2018

object Day13Part2 extends Part {

  override def answer: String = {
    //val collision = Day13.System.build(Day13.getTestData2).findLastRemainingCart
    val collision = Day13.System.build(Day13.getInput).findLastRemainingCart
    collision.x + "," + collision.y
  }

}
