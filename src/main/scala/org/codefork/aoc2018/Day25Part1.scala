package org.codefork.aoc2018

object Day25Part1 extends Part {

  override val validateTestData = true

  override def assertTestCases(): Unit = {
    val test1 = Day25.getInput("/day25/testData1.txt")
    assert(Day25.countConstellations(test1) == 4)

    val test2 = Day25.getInput("/day25/testData2.txt")
    assert(Day25.countConstellations(test2) == 3)

    val test3 = Day25.getInput("/day25/testData3.txt")
    assert(Day25.countConstellations(test3) == 8)
  }

  override def answer: String = {
    val input = Day25.getInput("/day25/input.txt")
    Day25.countConstellations(input).toString
  }

}
