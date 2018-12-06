package org.codefork.aoc2018

object Day4Part2 extends Part {

  case class GuardIdMinuteFreq(guardId: Int = -1, minute: Int = -1, freq: Int = -1)

  case class MinuteFreq(minute: Int = -1, freq: Int = -1)

  def main(args: Array[String]): Unit = println(answer)

  override def answer: String = {
    val shifts = Day4.getShifts

    val grouped = shifts.groupBy(_.guardId)

    val result = grouped.foldLeft(GuardIdMinuteFreq()) { case (acc, (guardId, shifts)) => {
      val minuteToCounts = Day4.minuteToCounts(shifts)
      val mostFreq = minuteToCounts.foldLeft(MinuteFreq()) { case (acc, (minute, freq)) => {
        if(freq > acc.freq) {
          MinuteFreq(minute, freq)
        } else {
          acc
        }
      }}

      if(mostFreq.freq > acc.freq) {
        GuardIdMinuteFreq(guardId, mostFreq.minute, mostFreq.freq)
      } else {
        acc
      }
    }}

    (result.guardId * result.minute).toString
  }

}
