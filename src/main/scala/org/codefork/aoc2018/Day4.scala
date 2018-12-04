package org.codefork.aoc2018

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.io.Source

object Day4 {

  case class SleepPeriod(val startMin: Int, val endMin: Int)

  case class Shift(val guardId: Int,
                   val date: Date,
                   val startTime: String,
                   val sleepPeriods: Seq[SleepPeriod]) {
    val minsAsleep = sleepPeriods.foldLeft(0) { (acc, per) =>
      acc + (per.endMin - per.startMin)
    }
  }

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  def getShifts = {
    val url = getClass.getResource("/day4/input.txt")
    Source.fromURL(url).getLines().toSeq.sorted.foldLeft(Seq[Shift]()) {
      (acc, line) =>
        val time = line.substring(12, 17)

        val dateOriginal = dateFormat.parse(line.substring(1, 11))
        val date = if (time.startsWith("23")) {
          val cal = Calendar.getInstance
          cal.setTime(dateOriginal)
          cal.add(Calendar.DATE, 1)
          cal.getTime()
        } else {
          dateOriginal
        }

        val event = line.substring(19)
        if (event.startsWith("Guard")) {
          val guardId = event.split(" ")(1).replace("#", "").toInt
          acc :+ Shift(guardId, date, time, Seq[SleepPeriod]())
        } else if (event == "falls asleep") {
          val sleepPeriod =
            SleepPeriod(startMin = time.substring(3, 5).toInt, endMin = -1)
          val lastShift = acc.last
          Util.replaceLast(
            acc,
            lastShift.copy(
              sleepPeriods = lastShift.sleepPeriods :+ sleepPeriod))
        } else if (event == "wakes up") {
          val lastShift = acc.last
          val modSleepPeriod = lastShift.sleepPeriods.last
            .copy(endMin = time.substring(3, 5).toInt - 1)
          val newSleepPeriods =
            Util.replaceLast(lastShift.sleepPeriods, modSleepPeriod)
          Util.replaceLast(acc, lastShift.copy(sleepPeriods = newSleepPeriods))
        } else {
          acc
        }
    }
  }

  // frequency that guard was asleep for a given minute
  def minuteToCounts(shifts: Seq[Shift]) = {
    shifts.foldLeft(Map[Int, Int]()) { (acc, shift) =>
      val forPeriod = shift.sleepPeriods.foldLeft(Map[Int, Int]()) {
        (acc, per) =>
          per.startMin.to(per.endMin).foldLeft(acc) { (acc, min) =>
            acc ++ Map(min -> 1)
          }
      }
      Util.mergeMapsSummingValues(acc, forPeriod)
    }
  }

}
