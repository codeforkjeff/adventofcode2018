package org.codefork.aoc2018

import org.codefork.aoc2018.Day16.Registers

object Day19Part2 extends Part {

  override def answer: String = {
    //val result = Day19.getProgram(Registers(Seq(1,0,0,0,0,0))).execute
    //result.registers.get(0).toString

    // instr 8 changes reg 1 to 10551289
    // instr 9 triggers register 5 to be incremented from 0 to 1
    // instr 10 increments reg 2, causing skip over instr 11
    // instr 12 adds value 1 to reg 4
    // instr 13 flips reg 5 back to 0
    // instr 14 keeps reg 2 the same

    //val result = Day19.getProgram(Registers(Seq(0,10551287,9,10551288,1,0)), ip = 9).execute
    //val result = Day19.getProgram(Registers(Seq(1,10551288,9,10551288,2,0)), ip = 9).execute

    // instr 3 multiples and instr 4 tests for equality
    //val result = Day19.getProgram(Registers(Seq(1,5275644,3,10551288,2,0)), ip = 3).execute

    // instr 7 changes reg 0

    // registers 0 and 4 change in alternating cycles
    // when reg 0 gets incremented by value of reg 4, reg 1 does NOT reset to 1 but keeps going
    // if divisible, instr 4 causes reg 5 to be set to 1, then instr 6 causes a jump to 7

    // instr 9 is what breaks out of the program, when reg 1 value > 10551288


    //val result = Day19.getProgram(Registers(Seq(3,10551287,9,10551288,2,0)), ip = 9).execute

    //val result = Day19.getProgram(Registers(Seq(3,3517096,3,10551288,3,0)), ip = 3).execute

//    val result = Day19.getProgram(Registers(Seq(6,10551288,3,10551288,4,0)), ip = 3).execute

//    val result = Day19.getProgram(Registers(Seq(6,2637821,9,10551288,4,0)), ip = 9).execute

   // val result = Day19.getProgram(Registers(Seq(10,10551287,3,10551288,4,0)), ip = 3).execute

    //val result = Day19.getProgram(Registers(Seq(10,10551287,9,10551288,5,0)), ip = 9).execute

    //val result = Day19.getProgram(Registers(Seq(10,1758548,3,10551288,6,0)), ip = 3).execute

//    val result = Day19.getProgram(Registers(Seq(16,1758548,3,10551288,6,0)), ip = 3).execute

//    val result = Day19.getProgram(Registers(Seq(22,1758548,3,10551288,6,0)), ip = 3).execute

//    val result = Day19.getProgram(Registers(Seq(28,1758548,3,10551288,6,0)), ip = 3).execute

//    val result = Day19.getProgram(Registers(Seq(28,10551287,9,10551288,6,0)), ip = 9).execute

//    val result = Day19.getProgram(Registers(Seq(28,10551287,9,10551288,7,0)), ip = 9).execute

//    val result = Day19.getProgram(Registers(Seq(28,1318911,3,10551288,8,0)), ip = 3).execute

//    val result = Day19.getProgram(Registers(Seq(36,10551287,9,10551288,8,0)), ip = 9).execute

//    val result = Day19.getProgram(Registers(Seq(36,959207,3,10551288,11,0)), ip = 3).execute

    val result = Day19.getProgram(Registers(Seq(47,959207,3,10551288,11,0)), ip = 3).execute

    // on this run, instr 7 change reg 0 to 10551289
    //val result = Day19.getProgram(Registers(Seq(1, 10551288, 9, 10551288, 10551287, 0)), ip = 9).execute


    // instr 3 multiples reg 4 by reg 1
    //val result = Day19.getProgram(Registers(Seq(1, 1, 3, 10551288, 10551288, 0)), ip = 3).execute


    //val result = Day19.getProgram(Registers(Seq(10551289, 10551288, 9, 10551288, 10551288, 0)), ip = 9).execute
    //10551289 is too low

    //ping-pongs between changing t streg 1 and incrementing reg 4

    result.registers.get(0).toString
  }

}
