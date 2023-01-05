package org.codefork.aoc2018

object Day19Part1 extends Part {

  override def answer: String = {
    val result = Day19.getProgram("/day19/input.txt").execute
    result.registers.get(0).toString
  }

}
