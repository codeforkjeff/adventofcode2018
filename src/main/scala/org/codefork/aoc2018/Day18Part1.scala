package org.codefork.aoc2018

object Day18Part1 extends Part {

  override def answer: String = {
    val initial = Day18.getInput.populateAdjacentCache()
    val after10 = 1.to(10).foldLeft(initial) { (acc, i) =>
      acc.transform
    }
    after10.resourceValue.toString
  }

}
