package org.codefork.aoc2018

import org.codefork.aoc2018.Day6.Coord

object Day6Part2 extends Part {

  override def answer: String = {
    val coords = Day6.getCoords

    // pick a point in the center, to start
    val x = (coords.maxBy(_.x).x - coords.minBy(_.x).x) / 2
    val y = (coords.maxBy(_.y).y - coords.minBy(_.y).y) / 2
    val seed = Coord(x, y)

    grow(seed, coords, Set(seed)).size.toString
  }

  // expand from seed until all the new candidates found during an iteration no longer
  // meet the criteria of having sum total dist < 10000
  def grow(seed: Coord,
           coords: Seq[Coord],
           area: Set[Coord],
           dist: Int = 1): Set[Coord] = {
    val newCandidates = seed.findAtDist(dist)
    val newArea = newCandidates.filter(c => {
      coords.map(_.distTo(c)).sum < 10000
    })
    if (newArea.size > 0) {
      grow(seed, coords, area ++ newArea, dist + 1)
    } else {
      area
    }
  }

}
