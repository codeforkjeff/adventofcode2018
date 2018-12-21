package org.codefork.aoc2018

import org.codefork.aoc2018.Day16.Registers

object Day16Part2 extends Part {

  override def answer: String = {
    val opcodeToNames = Day16.opcodeToNames

    val result =
      Day16.getInputTestProgram.foldLeft(Registers(Seq(0, 0, 0, 0))) {
        (acc, instruction) =>
          {
            val opname = opcodeToNames(instruction.opcode)
            acc.getOpByName(opname)(instruction.a, instruction.b, instruction.c)
          }
      }
    result.get(0).toString
  }

}
