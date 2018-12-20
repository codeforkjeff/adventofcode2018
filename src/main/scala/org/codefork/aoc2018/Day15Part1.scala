package org.codefork.aoc2018

object Day15Part1 extends Part {

  override def answer: String = {
    assert(Day15.buildBattlefield(Day15.getTestData1).fightToTheDeath().outcome == 27730)
    assert(Day15.buildBattlefield(Day15.getTestData2).fightToTheDeath().outcome == 36334)
    assert(Day15.buildBattlefield(Day15.getTestData3).fightToTheDeath().outcome == 39514)
    assert(Day15.buildBattlefield(Day15.getTestData4).fightToTheDeath().outcome == 27755)
    assert(Day15.buildBattlefield(Day15.getTestData5).fightToTheDeath().outcome == 28944)
    assert(Day15.buildBattlefield(Day15.getTestData6).fightToTheDeath().outcome == 18740)
    Day15.buildBattlefield(Day15.getInput).fightToTheDeath().outcome.toString
  }

}
