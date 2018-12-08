package org.codefork.aoc2018

object Day8Part1 extends Part {

  override def answer: String = {
    //val root = Day8.makeTree(Day8.inputToList(Day8.getTestData))
    val root = Day8.makeTree(Day8.inputToList(Day8.getInputData))
    Day8.tallyMetadata(Seq(root)).toString
  }

}
