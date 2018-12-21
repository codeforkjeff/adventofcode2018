package org.codefork.aoc2018

object Day16Part1 extends Part {

  override def answer: String = {
    Day16.getInputSamples.count(sample => sample.getOpsMatchingAfter.size >= 3).toString
  }

}
