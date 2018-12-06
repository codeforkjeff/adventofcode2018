package org.codefork.aoc2018

object Day3Part1 extends Part {

  def main(args: Array[String]): Unit = println(answer)

  override def answer: String = {
    new Day3().overlaps.size.toString
  }
}
