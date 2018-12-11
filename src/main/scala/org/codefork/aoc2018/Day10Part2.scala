package org.codefork.aoc2018

import org.codefork.aoc2018.Day10.{CoordSet, getCoords}

object Day10Part2 extends Part {

  override def answer: String = {
    Day10.findLetters(CoordSet(getCoords)).t.toString
  }

}
