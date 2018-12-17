package org.codefork.aoc2018

import org.codefork.aoc2018.Day14.RecipeHistory

object Day14Part2 extends Part {

  def findFirstOccurrence(target: String, recipeHistory: RecipeHistory) = {

    val result = recipeHistory.iterateUntil(r =>
      // an iteration can result in 1 or 2 digits being appended, so
      // we test accordingly
      r.lastN(target.size + 1).mkString.contains(target)
    )

    val indexInBiggerChunk = result.lastN(target.size + 1).mkString.indexOf(target)

    (result.q.size - target.size) + (if (indexInBiggerChunk == 0) -1 else 0)
  }

  override def answer: String = {
    val recipeHistory = RecipeHistory(0, 1, 3, 7, Vector(3, 7))
    // println(find("51589", recipeHistory).toString)
    // println(find("01245", recipeHistory).toString)
    // println(find("92510", recipeHistory).toString)
    // println(find("59414", recipeHistory).toString)
    findFirstOccurrence("170641", recipeHistory).toString
  }

}
