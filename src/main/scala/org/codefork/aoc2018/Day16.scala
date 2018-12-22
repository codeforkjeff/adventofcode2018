package org.codefork.aoc2018

import scala.io.Source

object Day16 {

  type Op = (Int, Int, Int) => Registers

  case class Registers(values: Seq[Int]) {

    def get(r: Int) = values(r)

    def set(r: Int, value: Int): Registers =
      copy(values.patch(r, Seq(value), 1))

    def addr(a: Int, b: Int, c: Int): Registers = set(c, get(a) + get(b))

    def addi(a: Int, b: Int, c: Int): Registers = set(c, get(a) + b)

    def mulr(a: Int, b: Int, c: Int): Registers = set(c, get(a) * get(b))

    def muli(a: Int, b: Int, c: Int): Registers = set(c, get(a) * b)

    def banr(a: Int, b: Int, c: Int): Registers = set(c, get(a) & get(b))

    def bani(a: Int, b: Int, c: Int): Registers = set(c, get(a) & b)

    def borr(a: Int, b: Int, c: Int): Registers = set(c, get(a) | get(b))

    def bori(a: Int, b: Int, c: Int): Registers = set(c, get(a) | b)

    def setr(a: Int, b: Int, c: Int): Registers = set(c, get(a))

    def seti(a: Int, b: Int, c: Int): Registers = set(c, a)

    def gtir(a: Int, b: Int, c: Int): Registers =
      set(c, if (a > get(b)) 1 else 0)

    def gtri(a: Int, b: Int, c: Int): Registers =
      set(c, if (get(a) > b) 1 else 0)

    def gtrr(a: Int, b: Int, c: Int): Registers =
      set(c, if (get(a) > get(b)) 1 else 0)

    def eqir(a: Int, b: Int, c: Int): Registers =
      set(c, if (a == get(b)) 1 else 0)

    def eqri(a: Int, b: Int, c: Int): Registers =
      set(c, if (get(a) == b) 1 else 0)

    def eqrr(a: Int, b: Int, c: Int): Registers =
      set(c, if (get(a) == get(b)) 1 else 0)

    def getOpsMatchingResult(instruction: Instruction,
                             /*opcode: Int,
                             argA: Int,
                             argB: Int,
                             argC: Int,*/
                             result: Registers): Seq[String] =
      allOps
        .map {
          case (opname, op) =>
            (opname, op(instruction.a, instruction.b, instruction.c))
        }
        .filter(r => r._2 == result)
        .map(_._1)
        .toSeq

    def getOpByName(opname: String) = allOps(opname)

    def allOps =
      Map[String, Op](
        "addr" -> addr,
        "addi" -> addi,
        "mulr" -> mulr,
        "muli" -> muli,
        "banr" -> banr,
        "bani" -> bani,
        "borr" -> borr,
        "bori" -> bori,
        "setr" -> setr,
        "seti" -> seti,
        "gtir" -> gtir,
        "gtri" -> gtri,
        "gtrr" -> gtrr,
        "eqir" -> eqir,
        "eqri" -> eqri,
        "eqrr" -> eqrr
      )

  }

  case class Instruction(opcode: Int, a: Int, b: Int, c: Int)

  case class Sample(instruction: Instruction,
                    before: Registers,
                    after: Registers) {

    def getOpsMatchingAfter =
      before.getOpsMatchingResult(instruction, after)

  }

  def parseRegisters(s: String) = {
    val sub = s.substring(s.indexOf("[") + 1, s.indexOf("]"))
    val nums = sub.split(",").toList.map(_.trim).map(_.toInt)
    Registers(nums)
  }

  def getInputSamples: Seq[Sample] = {
    val url = getClass.getResource("/day16/input.txt")
    val lines = Source.fromURL(url).getLines().toSeq
    val groups = lines.grouped(4).filter(g => g.head.startsWith("Before"))
    groups
      .map(g => {
        val before = parseRegisters(g.head)
        val instructionParts = g(1).split(" ").map(_.toInt)
        val after = parseRegisters(g(2))
        Sample(Instruction(instructionParts(0),
                           instructionParts(1),
                           instructionParts(2),
                           instructionParts(3)),
               before,
               after)
      })
      .toSeq
  }

  def getInputTestProgram: Seq[Instruction] = {
    val url = getClass.getResource("/day16/input.txt")
    val lines = Source.fromURL(url).getLines().toSeq
    val iLastAfter = lines.lastIndexWhere(line => line.startsWith("After"))
    val testProgram = lines.slice(iLastAfter + 4, lines.size)
    testProgram.map(line => {
      val parts = line.split(" ").map(_.toInt)
      Instruction(parts(0), parts(1), parts(2), parts(3))
    })
  }

  // recursively deduce by removing opcodes already determined with certainty
  // from other sets, until every set has just 1 opcode
  def deduce(possibilities: Map[Int, Set[String]]): Map[Int, String] = {

    val determined = possibilities.values
      .filter(_.size == 1)
      .map(_.head)
      .toSet

    val newPossibilities = possibilities
      .map {
        case (opcode, opset) =>
          (opcode -> (if (opset.size > 1) opset.diff(determined) else opset))
      }

    if (newPossibilities.exists { case (_, opset) => opset.size > 1 }) {
      deduce(newPossibilities)
    } else {
      newPossibilities.map { case (opcode, opset) => (opcode -> opset.head) }
    }
  }

  def opcodeToNames: Map[Int, String] = {
    val possibilities =
      Day16.getInputSamples.foldLeft(Map[Int, Set[String]]()) { (acc, sample) =>
        {
          val results = sample.getOpsMatchingAfter.toSet
          val ops = acc.getOrElse(sample.instruction.opcode, results)
          val newOps = ops.intersect(results)
          acc + (sample.instruction.opcode -> newOps)
        }
      }
    deduce(possibilities)
  }

}
