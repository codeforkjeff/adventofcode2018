package org.codefork.aoc2018

object Day13Part1 extends Part {

  override def answer: String = {
    //val collision = Day13.System.build(Day13.getTestData).findFirstCollision
    val collision = Day13.System.build(Day13.getInput).findFirstCollision
    // website will only accept answer with no spaces
    s"${collision.x},${collision.y}"
  }

}
