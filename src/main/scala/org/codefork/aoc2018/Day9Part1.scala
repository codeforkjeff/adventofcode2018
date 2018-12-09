package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day9Part1 extends Part {

  case class Game(numPlayers: Int, lastMarblePoints: Int) {

    def findHighestScore: Int = calculateFinalScores().values.max

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

          //println(player + " scored " + score)
          //println("marble at -7 was " + circle(indexToRemove))

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

  override def answer: String = {
    //val input = getTestData
    val input = getInputData
    val matches = raw"\d+".r.findAllMatchIn(input).toList.map(_.toString.toInt)
    Game(matches(0), matches(1)).findHighestScore.toString
  }

}
