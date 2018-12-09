package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

case class Device(val seenFreq: Set[Int],
                  val currentFreq: Int,
                  val firstFreqReachedTwice: Option[Int]) {

  def updateDevice(change: Int): Device = {
    if (firstFreqReachedTwice.isEmpty) {
      val newFreq = currentFreq + change
      val newSeen = seenFreq + newFreq
      if (seenFreq.contains(newFreq)) {
        copy(newSeen, newFreq, Some(newFreq))
      } else {
        copy(newSeen, newFreq)
      }
    } else {
      this
    }
  }

  @tailrec
  final def calibrateUntilFreqRepeats: Device = {
    val url = getClass.getResource("/day1/input.txt")
    val s = Source.fromURL(url)
    val result = s
      .getLines()
      .foldLeft(this) { (device, item) =>
        device.updateDevice(item.toInt)
      }
    if (result.firstFreqReachedTwice.isEmpty) {
      result.calibrateUntilFreqRepeats
    } else {
      result
    }
  }
}

object Day1Part2 extends Part {

  override def answer: String = {
    val device = Device(Set[Int](0), 0, None)
    val deviceResult = device.calibrateUntilFreqRepeats
    deviceResult.firstFreqReachedTwice.get.toString
  }
}
