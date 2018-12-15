package org.codefork.aoc2018

import scala.io.Source

object Day12Part1 extends Part {

  def nextGen(state: String, pos: Int, rules: Map[String, String]) = {
    val slice = state.substring(pos - 2, pos + 3)
    val result =
      if (rules.contains(slice))
        rules(slice)
      else "."
    result
  }

  def trimTrailingPeriods(s: String) = s.reverse.dropWhile(_ == '.').reverse

  override def answer: String = {
    val url = getClass.getResource("/day12/input.txt")

    val lines = Source.fromURL(url).getLines().toSeq

    lines.head
      .replace("initial state: ", "")
      .zipWithIndex
      .foldLeft(Set[Int]()) {
        case (acc, (char, index)) => {
          acc ++ (if (char == "#") Set(index) else Set.empty)
        }
      }

    val initial = lines.head.replace("initial state: ", "")
    val rules = lines
      .drop(2)
      .map(line => line.substring(0, 5) -> line.last.toString)
      .toMap

    val result = 1.to(20).foldLeft((0, initial)) { case ((startingPotNumberOfState, state), i) =>
      {
        val padded = "...." + state + "...."
        // result of next gen will be have 4 slots than padded
        val t1 = 2
          .to(padded.size - 3)
          .map(pos => {
            nextGen(padded, pos, rules)
          })
          .mkString
        val t2 = t1.dropWhile(ch => ch == '.')
        val newStartingPotNumberOfState = startingPotNumberOfState - 2 + (t1.length - t2.length)
        val t3 = trimTrailingPeriods(t2)
        //println(i, (newStartingPotNumberOfState, t3))
        (newStartingPotNumberOfState, t3)
      }
    }

    val count = result._2.foldLeft((result._1, 0)) {
      case ((potNumber, acc), char) => {
        val newAcc = acc + (if (char == '#') potNumber else 0)
        (potNumber + 1, newAcc)
      }
    }

    count._2.toString
  }

}
