package org.codefork.aoc2018

object Day19Part1 extends Part {

  override def answer: String = {
    val result = Day19.getProgram().execute
    result.registers.get(0).toString
  }

}
