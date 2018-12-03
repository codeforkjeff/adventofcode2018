package org.codefork.aoc2018

case class BoxId(id: String) {
  def getLetterCounts: Map[Char, Int] =
    id.foldLeft(Map[Char, Int]()) { (counts, letter) =>
      counts + (letter -> (counts.getOrElse(letter, 0) + 1))
    }

}
