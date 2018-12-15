package org.codefork.aoc2018

import scala.io.Source

object Day12 {

  def getInput: Pots = {

    val url = getClass.getResource("/day12/input.txt")

    val lines = Source.fromURL(url).getLines().toSeq

    lines.head
      .replace("initial state: ", "")
      .zipWithIndex
      .foldLeft(Set[Int]()) {
        case (acc, (char, index)) => {
          acc ++ (if (char == '#') Set(index) else Set.empty)
        }
      }

    val initial = lines.head.replace("initial state: ", "")
    val rules = lines
      .drop(2)
      .map(line => line.substring(0, 5) -> line.last.toString)
      .toMap

    Pots(initial, 0, rules)
  }

  case class Pots(state: String,
                  startingPotNumber: Int,
                  rules: Map[String, String]) {

    val padded = "...." + state + "...."

    def trimTrailingPeriods(s: String) = s.reverse.dropWhile(_ == '.').reverse

    def nextGenForPot(paddedPos: Int) = {
      val slice = padded.substring(paddedPos - 2, paddedPos + 3)
      val result =
        if (rules.contains(slice))
          rules(slice)
        else "."
      result
    }

    def nextGen = {
      // result of next gen will be have 4 slots than padded
      val t1 = 2
        .to(padded.size - 3)
        .map(paddedPos => nextGenForPot(paddedPos))
        .mkString
      val t2 = t1.dropWhile(ch => ch == '.')
      val newStartingPotNumber = startingPotNumber - 2 + (t1.length - t2.length)
      val t3 = trimTrailingPeriods(t2)
      copy(state = t3, startingPotNumber = newStartingPotNumber)
    }

    def sumPotNumbers =
      state
        .foldLeft((startingPotNumber, 0)) {
          case ((potNumber, acc), char) => {
            val newAcc = acc + (if (char == '#') potNumber else 0)
            (potNumber + 1, newAcc)
          }
        }
        ._2

    def sumPotNumbersAtGeneration(i: Int) = {
      val result = 1.to(i).foldLeft(this)((pots, i) => pots.nextGen)
      result.sumPotNumbers
    }

  }

}
