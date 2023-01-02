package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day24Part1 extends Part {

  override def answer: String = {
    val reindeerState = Day24.parseInput("/day24/input.txt")

    val result = Day24.fightUntilWin(reindeerState)
    result.unitsOfWinningArmy.toString
  }

}
