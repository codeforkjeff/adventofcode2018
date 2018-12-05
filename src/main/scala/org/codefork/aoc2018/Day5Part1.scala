package org.codefork.aoc2018

object Day5Part1 {

  def main(args: Array[String]): Unit = {
    val s = Day5.getInputPolymer
    val reduced = Day5.reactPolymer(s)
    println(reduced.length)
  }

}
