package org.codefork.aoc2018

object Day12Part1 extends Part {

  override def answer: String = {
    Day12.getInput.sumPotNumbersAtGeneration(20).toString
  }

}
