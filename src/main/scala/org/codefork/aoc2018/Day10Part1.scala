package org.codefork.aoc2018

import org.codefork.aoc2018.Day10.{CoordSet, getCoords}

object Day10Part1 extends Part {

  override def answer: String =
    "\n" + CoordSet(getCoords).findLetters().render

}
