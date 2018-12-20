package org.codefork.aoc2018

import org.codefork.aoc2018.Day15.Battlefield

object Day15Part2 extends Part {

  def untilNoElvesDie(battlefield: Battlefield, elfAttackPower: Int = 4): Int = {
    println("trying elf attack power " + elfAttackPower)
    val finished = battlefield.fightToTheDeath(elfAttackPower)
    if(finished.elves.size == battlefield.elves.size) {
      finished.outcome
    } else {
      untilNoElvesDie(battlefield, elfAttackPower + 1)
    }
  }

  override def status: Status = NeedsOptimization

  override def answer: String = {
    //untilNoElvesDie(Day15.getTestData1).toString
    //untilNoElvesDie(Day15.getTestData3).toString
    val battlefield = Day15.buildBattlefield(Day15.getInput)
    untilNoElvesDie(battlefield).toString
  }

}
