package org.codefork.aoc2018

object Day22Part2 extends Part {

  override def answer: String = {

//    val testCaveMap = Day22.CaveMap(510, XY(10,10))
//    testCaveMap.shortestPath().toString

    Day22.buildCaveMapFromInput().shortestPath().toString
  }

}
