package org.codefork.aoc2018

import scala.io.Source

object Day7 {

  case class Step(letter: Char, children: Seq[Char])

  def linesToDependencies(lines: Seq[String]) =
    lines
      .map(line => (line.charAt(5), line.charAt(36)))
      .foldLeft(Map[Char, String]()) {
        case (acc, (from, to)) => {
          if (acc.contains(from)) {
            val children = acc(from)
            acc + (from -> (children + to))
          } else {
            acc + (from -> to.toString)
          }
        }
      }

  def linesToPrereqs(lines: Seq[String]) =
    lines
      .map(line => (line.charAt(5), line.charAt(36)))
      .foldLeft(Map[Char, String]()) {
        case (acc, (from, to)) => {
          if (acc.contains(to)) {
            val children = acc(to)
            acc + (to -> (children + from))
          } else {
            acc + (to -> from.toString)
          }
        }
      }

  val testData = Seq(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."
  )

  val testDataDeps = linesToDependencies(testData)

  val testDataPrereqs = linesToPrereqs(testData)

  val getInput: Seq[String] = {
    val url = getClass.getResource("/day7/input.txt")
    Source
      .fromURL(url)
      .getLines()
      .toSeq
  }

  def getDependencies =
    linesToDependencies(getInput)

  def getPrereqs: Map[Char, String] =
    linesToPrereqs(getInput)

}
