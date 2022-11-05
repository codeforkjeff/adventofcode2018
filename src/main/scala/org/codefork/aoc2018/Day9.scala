package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  // Map of player numbers to scores
  // player is 0-indexed instead of 1-indexed as in example
  type Scores = Map[Int, Long]

  case class Result(scores: Scores, marbleCounter: Int) {
    def findHighestScore = scores.values.max
  }

  case class Game(numPlayers: Int, lastMarblePoints: Int) {

    /**
     * This solution works for Part 1 but is too slow for Part 2
     * (never managed to run to completion). The operations on Vector
     * are too slow.
     *
     * TODO: try implementing circle as a set of two Lists (stacks)
     * with redundant data, to make it faster to do circle operations
     * while still keeping everything immutable
     */
    @tailrec
    final def calculateFinalScores(player: Int = 0,
                                   circle: Vector[Long] = Vector(0),
                                   indexOfCurrentMarble: Int = 0,
                                   marbleCounter: Int = 0,
                                   scores: Scores = Map.empty): Result = {
      if (marbleCounter == lastMarblePoints) {
        Result(scores, marbleCounter)
      } else {

        val nextPlayer = (player + 1) % numPlayers

        val toPlace = marbleCounter + 1

        if (toPlace % 23 == 0) {
          val indexToRemove = {
            val minus7 = indexOfCurrentMarble - 7
            if (minus7 < 0) minus7 + circle.size else minus7
          }

          val score = toPlace + circle(indexToRemove)

          val newScore = scores.getOrElse(player, 0L) + score
          val newScores = scores + (player -> newScore)

          val newCircle = circle.patch(indexToRemove, List.empty, 1)

//          println(
//            "score i = " + ((toPlace / 23) - 1) + " player " + player + " has new score " + newScore + " (+" + score + ") with marble " + toPlace
//               + ", marble removed = " + circle(indexToRemove) + " newCurrent=" + newCircle(indexToRemove) +
//               " circle size was = " + circle.size)

          // "The marble located immediately clockwise of the marble
          // that was removed becomes the new current marble."
          calculateFinalScores(nextPlayer,
            newCircle,
            indexToRemove,
            marbleCounter + 1,
            newScores)
        } else {

          // this inserts at start of list instead of at end, so it looks diff from example
          // on website, but it's effectively the same thing
          val indexToInsert =
          if (circle.size == 1) 1
          else (indexOfCurrentMarble + 2) % circle.size

          val newCircle = circle.patch(indexToInsert, Seq(toPlace.toLong), 0)

//          println("inserted " + toPlace + " marble at index = " + indexToInsert + ", newWrapCounter = " + newWrapCounter)

          calculateFinalScores(nextPlayer,
            newCircle,
            indexToInsert,
            marbleCounter + 1,
            scores
          )
        }
      }
    }

    class CDLLNode[T](val value: T, var prev: Option[CDLLNode[T]] = None, var next: Option[CDLLNode[T]] = None)

    /**
     * Implement a Circle as a mutable circular doubly linked list
     */
    class Circle[T](initialValue: T) {
      var currentMarble: CDLLNode[T] = CDLLNode[T](initialValue)
      currentMarble.prev = Some(currentMarble)
      currentMarble.next = Some(currentMarble)

      // insert a new node, replacing currentMarble
      def push(newValue: T): Unit = {
        val newHead = CDLLNode(newValue, currentMarble.prev, Some(currentMarble))
        newHead.prev.get.next = Some(newHead)
        currentMarble.prev = Some(newHead)
        currentMarble = newHead
      }

      final def moveCurrent(n: Int, node: Option[CDLLNode[T]] = None): Unit =
        if(n==0)
          currentMarble = node.get
        else {
          if(n > 0)
            moveCurrent(n - 1, node.getOrElse(currentMarble).next)
          else
            moveCurrent(n + 1, node.getOrElse(currentMarble).prev)
        }

      def pop(): T = {
        val popped = currentMarble.value

        val newHead = currentMarble.next.get
        newHead.prev = currentMarble.prev
        newHead.prev.get.next = Some(newHead)

        currentMarble.next = None
        currentMarble.prev = None

        currentMarble = newHead
        popped
      }

      // for debugging
      def output(): Unit = {
        var ptr: CDLLNode[T] = currentMarble
        while {
          print(ptr.value)
          print(" ")
          ptr = ptr.next.get
          ptr != currentMarble
        } do()
        println("\n")
      }

      def destroy(): Unit = {
        var ptr: CDLLNode[T] = currentMarble
        while {
          var ptrNext = ptr.next.get
          ptr.next = None
          ptr.prev = None
          ptr = ptrNext
          !ptr.prev.isEmpty
        } do ()
      }
    }

    /**
     * This solution uses mutable Circle which takes only a few seconds to run for Part 2.
     */
    @tailrec
    final def calculateFinalScoresCDLL(player: Int = 0,
                                       circle: Circle[Long] = Circle[Long](0L),
                                       marbleCounter: Int = 0,
                                       scores: Scores = Map.empty): Result = {
      if (marbleCounter == lastMarblePoints) {
        circle.destroy()
        Result(scores, marbleCounter)
      } else {
        val nextPlayer = (player + 1) % numPlayers

        val toPlace = marbleCounter + 1

        if (toPlace % 23 == 0) {
          circle.moveCurrent(-7)

          // "The marble located immediately clockwise of the marble
          // that was removed becomes the new current marble."
          val popped = circle.pop()

          val score = toPlace.toLong + popped

          val newScore = scores.getOrElse(player, 0L) + score
          val newScores = scores + (player -> newScore)

//          println(
//            "player " + player + " has new score " + newScore + " (+" + score + ") with marble " + toPlace + ", popped = " + popped)

          calculateFinalScoresCDLL(nextPlayer,
            circle,
            marbleCounter + 1,
            newScores)
        } else {
          circle.moveCurrent(2)
          circle.push(toPlace)

          calculateFinalScoresCDLL(nextPlayer,
            circle,
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
