package org.codefork.aoc2018

import scala.io.Source

object Day2Part1 {
  type LetterCounts = Map[Char, Int]

  case class BoxId(id: String) {
    def getLetterCounts: Map[Char, Int] =
      id.foldLeft(Map[Char, Int]()) { (counts, letter) =>
        counts + (letter -> (counts.getOrElse(letter, 0) + 1))
      }

  }

  def main(args: Array[String]) = {
    val url = getClass.getResource("/day2/input.txt")
    val s = Source.fromURL(url)
    val boxIdCounts = s
      .getLines()
      .foldLeft(Map[Int, Int]()) { (boxIdCounts, line) =>
        val boxId = BoxId(line)
        letterCountsToFrequencies(boxId.getLetterCounts)
          .foldLeft(boxIdCounts) { (acc, letterCount) =>
            acc + (letterCount -> (acc.getOrElse(letterCount, 0) + 1))
          }
      }
    val checksum = boxIdCounts(2) * boxIdCounts(3)
    println(s"checksum: $checksum")
  }

  // returns set of unique letter frequency counts
  // e.g. if an id has letter 'a' and letter 'b' occurring twice,
  // resulting set will have single item, int 2, in it (we only count frequencies once).
  def letterCountsToFrequencies(letterCounts: LetterCounts): Set[Int] =
    letterCounts.foldLeft(Set[Int]()) {
      case (acc, (letter, count)) =>
        acc + count
    }

}
