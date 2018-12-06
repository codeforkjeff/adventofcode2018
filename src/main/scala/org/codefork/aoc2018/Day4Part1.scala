package org.codefork.aoc2018

object Day4Part1 extends Part {

  override def answer: String = {
    val shifts = Day4.getShifts

    val grouped = shifts.groupBy(_.guardId)

    // find guard who spent most minutes asleep
    val guardIdAndTotalAsleep = grouped.foldLeft((-1, -1)) {
      case (acc, (guardId, shifts)) => {
        val totalAsleep = shifts.foldLeft(0) { (acc, shift) =>
          shift.minsAsleep + acc
        }
        if (totalAsleep > acc._2) {
          (guardId, totalAsleep)
        } else {
          acc
        }
      }
    }

    val guardId = guardIdAndTotalAsleep._1
    //println(s"guardId: $guardId")

    // What minute does that guard spend asleep the most?
    val minuteToCounts = Day4.minuteToCounts(grouped(guardId))

    val mostFrequencyMinute = minuteToCounts
      .foldLeft((-1, -1)) {
        case (acc, (min, count)) =>
          if (count > acc._2) {
            (min, count)
          } else {
            acc
          }
      }
      ._1
    //println(s"most freq min: $mostFrequencyMinute")

    (guardId * mostFrequencyMinute).toString
  }

}
