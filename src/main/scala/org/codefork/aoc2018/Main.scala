package org.codefork.aoc2018

object Main {

  val parts: Seq[Part] = Seq(
    Day1Part1,
    Day1Part2,
    Day2Part1,
    Day2Part2,
    Day3Part1,
    Day3Part2,
    Day4Part1,
    Day4Part2,
    Day5Part1,
    Day5Part2,
    Day6Part1,
    Day6Part2,
    Day7Part1
  )

  def main(args: Array[String]) = {
    val filtered =
      if (args.nonEmpty) { parts.filter(p => args.contains(Part.getClassName(p))) } else
        parts
    filtered.foreach(part => {
      part.run()
    })
  }

}
