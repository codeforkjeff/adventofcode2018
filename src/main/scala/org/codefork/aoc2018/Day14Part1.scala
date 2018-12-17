package org.codefork.aoc2018

import org.codefork.aoc2018.Day14.RecipeHistory

object Day14Part1 extends Part {

  def next10AtIteration(i: Int, seed: RecipeHistory) = {
    val stop = seed.iterateUntil(r => (r.q.size == i + 10))
    stop.lastN(10).mkString
  }

  override def answer: String = {
    val recipeHistory = RecipeHistory(0, 1, 3, 7, Vector(3, 7))
    // println(next10AtIteration(9, recipeHistory))
    // println(next10AtIteration(18, recipeHistory))
    // println(next10AtIteration(2018, recipeHistory))
    next10AtIteration(170641, recipeHistory)
  }

}
