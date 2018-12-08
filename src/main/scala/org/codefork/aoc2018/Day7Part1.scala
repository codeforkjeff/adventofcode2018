package org.codefork.aoc2018

object Day7Part1 extends Part {

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
    val deps = Day7.getDependencies
    // build initial set
    val initial = deps.keys.toSet.diff(deps.values.flatten.toSet).mkString
    orderSteps(initial, deps)
  }

}
