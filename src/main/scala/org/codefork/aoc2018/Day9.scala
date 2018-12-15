package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  case class Game(numPlayers: Int, lastMarblePoints: Int) {

    def findHighestScore: Int = calculateFinalScores().values.max

    // avoid creating/updating expensive circle data structure for every single turn;
    // just iterate on score-generating turns
    @tailrec
    final def calculateFinalScoresQuick(
        marbleToPlay: Int = 23,
        scores: Map[Int, Int] = Map.empty): Map[Int, Int] = {
//      if (marbleToPlay >= lastMarblePoints) {
      if (marbleToPlay >= 1000) {
        scores
      } else {
        // TODO: figure out what marble is removed
        val marbleAtCounterClockwise7 = 0

        // new current marble is at position -6 of marbleToPlay, which is always marble's value - 4
        val currentMarble = marbleToPlay - 4

        val score = marbleToPlay + marbleAtCounterClockwise7

        val player = (marbleToPlay - 1) % numPlayers

        val newScores = scores + (player -> (scores.getOrElse(player, 0) + score))

//        println(
//          "player " + player + " scored " + score + " with marble " + marbleToPlay)
//        println(
//          "index at -7 = " + 0 + ", marble = " + marbleAtCounterClockwise7 + " newCurrent=" + currentMarble)

        calculateFinalScoresQuick(marbleToPlay + 23, newScores)
      }
    }

    // player is 0-indexed instead of 1-indexed as in example
    @tailrec
    final def calculateFinalScores(player: Int = 0,
                                   circle: Vector[Int] = Vector(0),
                                   indexOfCurrentMarble: Int = 0,
                                   marbleCounter: Int = 0,
                                   scores: Map[Int, Int] = Map.empty,
                                   lastScore: Int = -1): Map[Int, Int] = {
      if (marbleCounter == lastMarblePoints) {
        scores
      } else {

        val nextPlayer = (player + 1) % numPlayers

        val toPlace = marbleCounter + 1

        if (toPlace % 23 == 0) {
          val indexToRemove = {
            val minus7 = indexOfCurrentMarble - 7
            if (minus7 < 0) minus7 + circle.size else minus7
          }

          val score = toPlace + circle(indexToRemove)

          val newScores = scores + (player -> (scores.getOrElse(player, 0) + score))

          val newCircle = circle.patch(indexToRemove, List.empty, 1)

//          println(
//            "player " + player + " scored " + score + " with marble " + toPlace)
//          println(
//            "index at -7 = " + indexToRemove + ", marble = " + circle(
//              indexToRemove) + " newCurrent=" + newCircle(indexToRemove))

          //println(player, newCircle)

          // "The marble located immediately clockwise of the marble
          // that was removed becomes the new current marble."
          calculateFinalScores(nextPlayer,
                               newCircle,
                               indexToRemove,
                               marbleCounter + 1,
                               newScores,
                               score)
        } else {

          // this inserts at start of list instead of at end, so it looks diff from example
          // on website, but it's effectively the same thing
          val indexToInsert =
            if (circle.size == 1) 1
            else (indexOfCurrentMarble + 2) % circle.size

          val newCircle = circle.patch(indexToInsert, Seq(toPlace), 0)

          //println(player, newCircle)

          calculateFinalScores(nextPlayer,
                               newCircle,
                               indexToInsert,
                               marbleCounter + 1,
                               scores)
        }
      }
    }
  }

  //val getTestData = "9 players; last marble is worth 32 points"
  //val getTestData = "10 players; last marble is worth 1618 points"
  //val getTestData = "13 players; last marble is worth 7999 points"

  val getInputData = {
    val url = getClass.getResource("/day9/input.txt")
    Source.fromURL(url).mkString.stripLineEnd
  }

}
