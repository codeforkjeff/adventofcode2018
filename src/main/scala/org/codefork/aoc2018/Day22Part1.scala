package org.codefork.aoc2018

object Day22Part1 extends Part {

  override def answer: String = {

//    val testCaveMap = Day22.CaveMap(510, XY(10,10))
//    assert(testCaveMap.calculateRisk == 114)

    Day22.buildCaveMapFromInput().calculateRisk.toString
  }

}
