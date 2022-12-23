package org.codefork.aoc2018

object Day23Part1 extends Part {

  override def answer: String = {
    val nanobots = Day23.getInput
    val strongest = nanobots.sortBy(_.r).reverse.head
    nanobots.count(bot =>
      bot.distanceTo(strongest) <= strongest.r
    ).toString
  }

}
