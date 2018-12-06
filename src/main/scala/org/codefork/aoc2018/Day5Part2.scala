package org.codefork.aoc2018

object Day5Part2 extends Part {

  case class Result(unitRemoved: String = "", resultingLen: Int = Int.MaxValue)

  def main(args: Array[String]): Unit = println(answer)

  override def answer: String = {
    val s = Day5.getInputPolymer
    val units = Day5.getDistinctUnits(s)
    // ugh, brute force, couldn't figure out a smarter way
    val result = units.foldLeft(Result()) { (result, unitLetter) =>
      {
        //println(s"Removing $unitLetter and reacting")
        val unitLetterRemoved = s.filter(_.toUpper != unitLetter)
        val reacted = Day5.reactPolymer(unitLetterRemoved)
        if (reacted.length < result.resultingLen) {
          Result(unitLetter.toString, reacted.length)
        } else {
          result
        }
      }
    }
    result.resultingLen.toString
  }

}
