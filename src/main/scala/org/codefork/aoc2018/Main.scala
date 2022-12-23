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
    Day7Part1,
    Day7Part2,
    Day8Part1,
    Day8Part2,
    Day9Part1,
    Day9Part2,
    Day10Part1,
    Day10Part2,
    Day11Part1,
    Day11Part2,
    Day12Part1,
    Day12Part2,
    Day13Part1,
    Day13Part2,
    Day14Part1,
    Day14Part2,
    Day15Part1,
    Day15Part2,
    Day16Part1,
    Day16Part2,
    Day17Part1,
    Day17Part2,
    Day18Part1,
    Day18Part2,
    Day19Part1,
    //Day19Part2
    Day20Part1,
    Day20Part2,
    // Day 21 depends on Day 19 which isn't finished
    Day22Part1,
    Day22Part2,
    Day23Part1,
  )

  def main(args: Array[String]) = {
    val filtered =
      if (args.nonEmpty) { parts.filter(p => args.contains(p.getClassName)) } else
        parts
    filtered.foreach(part => {
      part.status match
        case Incomplete => {
          println(part.getClassName + ": INCOMPLETE, skipping")
        }
        case NeedsOptimization => {
          println(
            part.getClassName + ": WARNING, needs optimization, this may take awhile")
          part.run()
        }
        case Finished => {
          part.run()
        }
    })
  }

}
