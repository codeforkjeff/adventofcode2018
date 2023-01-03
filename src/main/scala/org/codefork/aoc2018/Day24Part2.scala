package org.codefork.aoc2018

import org.codefork.aoc2018.Day24.ArmyType

import scala.annotation.tailrec

/**
 * Part 2 doesn't depend on any optimizations: it can be solved quickly
 * by a sequential search. There are two tricky aspects:
 *
 * 1. handling deadlocks where immune and infection armies end up in a state where they
 * can't do any more damage to one another, but no one has won. This only shows up in part 2
 * with boosted values.
 *
 * 2. the very complex rules for target selection have to be implemented EXACTLY:
 * there was a bug in my initial implementation where target candidates were incorrectly
 * removed from the pool when the attack damage was 0. This bug didn't show up in part 1
 * or in the test data, only part 2. I only figured this out after several painstaking hours
 * of comparing my logic to other solutions on reddit. :(
 */
object Day24Part2 extends Part {

  @tailrec
  def sequentialSearch(baseReindeerState: Day24.ReindeerState, boost: Int = 1): Day24.ReindeerState = {
    val testCase = baseReindeerState.immuneBoost(boost)
    val result = Day24.fightUntilWin(testCase)
    if(!result.deadlocked && result.reindeerState.winningArmy == Day24.ArmyType.ImmuneSystem)
      result.reindeerState
    else
      sequentialSearch(baseReindeerState, boost + 1)
  }

  override def answer: String = {
    val reindeerState = Day24.parseInput("/day24/input.txt")

    val finalReindeerState = sequentialSearch(reindeerState)
    finalReindeerState.immuneSystem.map(_.units).sum.toString
  }

}