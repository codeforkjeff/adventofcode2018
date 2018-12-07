package org.codefork.aoc2018

import scala.io.Source

object Day7Part1 extends Part {

  case class Step(letter: Char, children: Seq[Char]) {}

  def getDependencies = {
    val url = getClass.getResource("/day7/input.txt")
    Source
      .fromURL(url)
      .getLines()
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
  }

  def orderSteps(candidates: String, deps: Map[Char, String], ordered: String = ""): String = {
    if(candidates.size == 0) {
      ordered
    } else {
      val sorted = candidates.sorted
      val nextStep = sorted.head
      val remainingCandidates = sorted.tail
      // remove nextStep from deps
      val newDeps = deps.filterKeys(_ != nextStep)
      // only add children if nothing else is blocking child
      val children = deps.getOrElse(nextStep, "").filter(s =>
        !newDeps.values.mkString.contains(s))
      orderSteps(remainingCandidates + children, newDeps, ordered + nextStep)
    }
  }

  override def answer: String = {
    val deps = getDependencies
    // build initial set
    val initial = deps.keys.toSet.diff(deps.values.flatten.toSet).mkString
    orderSteps(initial, deps)
  }

}
