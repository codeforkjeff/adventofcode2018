package org.codefork.aoc2018

object Day5Part1 extends Part {

  def main(args: Array[String]): Unit = println(answer)

  override def answer: String = {
    val s = Day5.getInputPolymer
    val reduced = Day5.reactPolymer(s)
    reduced.length.toString
  }

}
