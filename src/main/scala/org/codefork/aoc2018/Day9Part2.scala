package org.codefork.aoc2018

object Day9Part2 extends Part {

  override def status = Incomplete

  override def answer: String = {
    //val input = getTestData
    val input = Day9.getInputData
    val matches = raw"\d+".r.findAllMatchIn(input).toList.map(_.toString.toInt)
    Day9.Game(matches(0), matches(1) * 100).calculateFinalScoresQuick()
    ""
  }

}
