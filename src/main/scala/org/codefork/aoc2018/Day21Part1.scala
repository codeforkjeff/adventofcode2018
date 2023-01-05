package org.codefork.aoc2018

/**
 * This seems to require the same analytical skills as Day 19 which
 * I gave up on.
 */
object Day21Part1 extends Part {

  override def answer: String = {
    val result = Day19.getProgram("/day21/input.txt").execute
    result.registers.get(0).toString
  }

}
