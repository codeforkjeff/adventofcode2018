package org.codefork.aoc2018

import org.codefork.aoc2018.Day23.{Bot, XYZ}

import scala.annotation.tailrec
import scala.collection.immutable.{Map, Seq}

/*
I tried a number of approaches and couldn't get any of them to work, though some of the answers
I came up with were pretty close.

The most promising approach started with finding the corner of the diamond-like shape
with the most bots in range, which turns out to be 874 bots. from there, we can try various methods
of walking or searching the space between that corner and the origin to find the closest point that's
still in range of all 874 bots.

The problem is that the space is too large for using brute force. I tried a binary search for points
along the edges of the cube, which yielded closer points but not the right answer.

Finally, I caved in, found this reddit thread, and implemented the overlapping ranges solution.
I had considered whether it's useful to calculate the shortest distance to origin of points
in a bot's range, but didn't actually come up with this solution myself.
https://www.reddit.com/r/adventofcode/comments/aa9uvg/day_23_aoc_creators_logic/

Eric Wastl said his own solution uses an octree. I should give that more thought and try it.
*/
object Day23Part2 extends Part {

  val origin = XYZ(0, 0, 0)

  case class Range(low: Long, high: Long) {
    // calculates the overlap with another range, returning an empty option
    // if there's no overlap
    def overlap(otherRange: Range): Option[Range] = {
      // order ranges by their low bound
      val r1 = if (low < otherRange.low) this else otherRange
      val r2 = if (low < otherRange.low) otherRange else this
      if (r2.low <= r1.high) {
        val newHigh = if (r2.high < r1.high) r2.high else r1.high
        Some(Range(r2.low, newHigh))
      } else {
        None
      }
    }
  }

  // A Corner is a coordinate and a Seq of bots within range of it
  case class Corner(point: XYZ, bot: Bot, inRange: Seq[Bot])

  // manhattan distance means the shape of a bot's range area is a diamond.
  // the overlap of two ranges necessarily involves a corner of each diamond.
  // so we use corners as our test points.
  def cornerInRangeOfMostBots(bots: Seq[Bot]): Corner = {

    val allCorners = bots.flatMap(bot => {
      bot.corners.map(cornerPoint => {
        Corner(cornerPoint, bot, Seq.empty)
      })
    })

    val corners = allCorners.map(corner =>
      corner.copy(inRange=bots.filter(_.withinRange(corner.point))))

    val sortedCorners = corners.sortBy(_.inRange.size).reverse.take(2)

    if(sortedCorners(0).inRange.size == sortedCorners(1).inRange.size) {
      println(s"${sortedCorners(0).inRange.size}")
      println(s"${sortedCorners(0).point}")
      println(s"${sortedCorners(1).inRange.size}")
      println(s"${sortedCorners(1).point}")
      println("warning: found more than 1 corner with same number of bots in range")
    }
    sortedCorners.head
  }

  // construct a rectilinear cube around each candidate to see if we need to keep testing candidates
  @tailrec
  def walk(candidates: Seq[XYZ], inRange: Seq[Bot], closestDistanceToOrigin: Long): Long = {
    //println(s"size of candidates=${candidates.size}")
    if(candidates.size <= 4) {
      println(s"candidates=${candidates}")
    }
    val closer = candidates
      .flatMap(_.containingBox(1))
      .distinct
      .filter(pt => inRange.forall(bot => bot.withinRange(pt)))
      .map(pt => (pt, pt.distanceTo(origin)))
      .filter(_._2 < closestDistanceToOrigin)
    if(closer.size > 0) {
      val newClosestDistanceToOrigin = closer.minBy(_._2)._2
      //println(s"newClosestDistanceToOrigin=${newClosestDistanceToOrigin}")
      val newCandidates = closer.filter(_._2 == newClosestDistanceToOrigin).map(_._1)
      walk(newCandidates, inRange, newClosestDistanceToOrigin)
    } else {
      closestDistanceToOrigin
    }
  }

  // binary search for point on edge that's in range of all bots and that's closest to origin
  def searchEdge(point1: XYZ, point2: XYZ, inRange: Seq[Bot]): XYZ = {
    println(s"${point1} ${point2}")
    if ((point1.x - point2.x) > 1) {
      val midpoint = XYZ(
        x = (point1.x + point2.x) / 2,
        y = (point1.y + point2.y) / 2,
        z = (point1.z + point2.z) / 2
      )
      if (inRange.forall(_.withinRange(midpoint))) {
        // midpoint is in range of all bots, so the answer is between midpoint and end
        searchEdge(midpoint, point2, inRange)
      } else {
        // midpoint is NOT in range of all bots, so the answer is between start and midpoint
        searchEdge(point1, midpoint, inRange)
      }
    } else {
      // test both
      if(inRange.forall(_.withinRange(point1))) {
        point1
      } else if(inRange.forall(_.withinRange(point2))) {
        point2
      } else {
        origin
      }
    }
  }

  // this takes too long and never finishes running
  def shortestDistanceByWalk(bots: Seq[Bot]): Long = {
     // the corner of the cube that's in range of the most bots
    val corner = cornerInRangeOfMostBots(bots)

    val distOfCornerToOrigin = corner.point.distanceTo(origin)

    println(s"corner=${corner.point}, ${corner.inRange.size} bots in range")

    //------------------------
    // this is the most "sound" approach I came up with, but takes too long and never finishes
    walk(Seq(corner.point), corner.inRange, corner.point.distanceTo(origin))
  }

  def shortestDistanceByRanges(bots: Seq[Bot]): Long = {
    // start by figuring out which bots overlap, by looking at corners
    val corner = cornerInRangeOfMostBots(bots)

    // calculate the distances of points closest and farthest from origin for each bot
    val ranges = corner.inRange.map(bot =>
      val dOrigin = bot.pos.distanceTo(origin)
      Range(dOrigin - bot.r, dOrigin + bot.r)
    )

    // reduce the ranges by finding overlaps in ranges
    ranges.reduce((acc, range) =>
      acc.overlap(range).getOrElse(acc)
    ).low
  }

  override def answer: String = {

    val testData = Seq(
      Bot(XYZ(10, 12, 12), 2),
      Bot(XYZ(12, 14, 12), 2),
      Bot(XYZ(16, 12, 12), 4),
      Bot(XYZ(14, 14, 14), 6),
      Bot(XYZ(50, 50, 50), 200),
      Bot(XYZ(10, 10, 10), 5)
    )

//    assert(shortestDistanceByRanges(testData) == 36)

    val input = Day23.getInput
    shortestDistanceByRanges(input).toString

    //113799030 is too low
    //113799249 is incorrect
    //117456628 is incorrect
    //120844716 is incorrect (found by walking from corner to center of smallest bot)
    //120860036 is incorrect
    //127905354 is too high
    //139997162 is too high
  }

}
