package org.codefork.aoc2018

import scala.annotation.tailrec

object Day14 {

  case class RecipeHistory(elf1index: Int,
                           elf2index: Int,
                           elf1val: Int,
                           elf2val: Int,
                           q: Vector[Int]) {

    @tailrec
    final def iterateUntil(cond: (RecipeHistory) => Boolean): RecipeHistory = {
      if (cond(this))
        this
      else {
        val newScore = elf1val + elf2val
        val digits = newScore.toString.map(digit =>
          Int.unbox(Integer.valueOf(digit.toString)))
        val newQ = q ++ Vector(digits: _*)

        val newElf1index = (elf1index + 1 + elf1val) % newQ.size
        val newElf2index = (elf2index + 1 + elf2val) % newQ.size

        val newRecipeHistory =
          RecipeHistory(newElf1index,
                        newElf2index,
                        newQ(newElf1index),
                        newQ(newElf2index),
                        newQ)
        newRecipeHistory.iterateUntil(cond)
      }
    }

    def lastN(n: Int) = q.takeRight(n)
  }

}
