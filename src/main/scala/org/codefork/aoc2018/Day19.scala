package org.codefork.aoc2018

import org.codefork.aoc2018.Day16.{NInstruction, Registers}

import scala.annotation.tailrec
import scala.io.Source

// note this re-uses Day 16 code
object Day19 {

  case class Program(instructions: Seq[NInstruction],
                     ipBoundToRegister: Int,
                     registers: Registers = Registers(Seq(0, 0, 0, 0, 0, 0)),
                     ip: Int = 0) {

    @tailrec
    final def execute: Program= {
      if(instructions.isDefinedAt(ip)) {
        val instr = instructions(ip)
        val r2 = registers.set(ipBoundToRegister, ip)
        val r3 = r2.execute(instr)
        println("ip = " + ip + " " + r2 + " " + instr + " " + r3)
        val newIp = r3.get(ipBoundToRegister) + 1
        copy(registers = r3, ip = newIp).execute
      } else
        this
    }

  }

  def getProgram(initialRegisters: Registers = Registers(Seq(0, 0, 0, 0, 0, 0)), ip: Int = 0) = {
    val url = getClass.getResource("/day19/input.txt")
    val lines = Source.fromURL(url).getLines().toSeq

    val ipBoundToRegister = lines.find(_.startsWith("#ip")).get.split(" ")(1).toInt

    val instructions = lines
      .filterNot(_.startsWith("#ip"))
      .map(line => {
        val pieces = line.split(" ").toSeq
        NInstruction(pieces(0),
                     pieces(1).toInt,
                     pieces(2).toInt,
                     pieces(3).toInt)
      })

    Program(instructions, ipBoundToRegister, registers = initialRegisters, ip = ip)
  }

}
