package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day20 {

  case class Room(x: Int, y: Int) {
    def direction(dir: Char): Room =
      if(dir == 'E') {
        copy(x + 1, y)
      } else if(dir == 'N') {
        copy(x, y + 1)
      } else if(dir == 'W') {
        copy(x - 1, y)
      } else if(dir == 'S') {
        copy(x, y - 1)
      } else {
        throw new Exception("unrecognized direction: " + dir)
      }
  }

  // breadth-first shortest path.
  // prev = rooms mapped to their previous rooms as visited by shortest path algorithm
  // orderedVisit = seq of rooms in order in which alg visited them
  // (we need this to find room farthest away)
  case class ShortestPath(prev: Map[Room, Room], orderedVisit: Seq[Room]) {

    @tailrec
    final def calcLevel(room: Room, level: Int): Int = {
      if (prev(room) == Room(0, 0)) {
        level + 1
      } else {
        calcLevel(prev(room), level + 1)
      }
    }

  }

  @tailrec
  final def buildShortestPath(elfMap: ElfMap,
                              queue: Queue[Room] = Queue(Room(0,0)),
                              visited: Seq[Room] = Seq(Room(0,0)),
                              prev: Map[Room, Room] = Map.empty,
                              lastConnected: List[Room] = List.empty): ShortestPath = {
    if(queue.nonEmpty) {
      val (v, remainder) = queue.dequeue
      val connected = elfMap.doors.getOrElse(v, List.empty).filter(!visited.contains(_))
      if (connected.length > 0) {
        val newQueue = queue.enqueue(connected)
        val newPrev = prev ++ connected.map(_ -> v).toMap
        val newVisited = visited ++ connected
        return buildShortestPath(elfMap, newQueue, newVisited, newPrev, connected)
      } else {
        return buildShortestPath(elfMap, remainder, visited, prev, lastConnected)
      }
    } else {
      return ShortestPath(prev, visited)
    }
  }

  case class ElfMap(doors: Map[Room, List[Room]]) {

    // connect two rooms with a door
    def connect(room1: Room, room2: Room): ElfMap = {
      val rooms = doors.getOrElse(room1, List.empty[Room])
      val roomsRev = doors.getOrElse(room2, List.empty[Room])
      // in both directions
      copy(doors ++ Map(
        room1 -> (rooms ++ List(room2)),
        room2 -> (roomsRev ++ List(room1))
      ))
    }

    def findFarthestDistance = {
      val shortestPath = buildShortestPath(this)
      shortestPath.calcLevel(shortestPath.orderedVisit.last, 0)
    }

    def countShortestDistancesAbove(threshold: Int) = {
      val shortestPath = buildShortestPath(this)
      val candidates = shortestPath.prev.keys.filter(_ != Room(0,0))
      candidates.count(c => {
        shortestPath.calcLevel(c, 0) >= threshold
      })
    }

  }

  @tailrec
  def buildElfMap(re: String,
                  elfMap: ElfMap = ElfMap(Map(Room(0,0) -> List.empty)),
                  current: Room = Room(0,0),
                  branchPoints: List[Room] = List.empty): ElfMap = {
    if(re.headOption.nonEmpty) {
      val step = re.head
      if(step == '^') {
        return buildElfMap(re.tail, elfMap, current, branchPoints)
      } else if(step == '(') {
        val newBranchPoints = current +: branchPoints
        return buildElfMap(re.tail, elfMap, current, newBranchPoints)
      } else if(step == '|') {
        val lastBranchPoint = branchPoints.head
        return buildElfMap(re.tail, elfMap, lastBranchPoint, branchPoints)
      } else if(step == ')') {
        val lastBranchPoint = branchPoints.head
        val newBranchPoints = branchPoints.tail
        return buildElfMap(re.tail, elfMap, lastBranchPoint, newBranchPoints)
      } else if(step == '$') {
        return elfMap
      } else if("NEWS".indexOf(step) > -1) {
        val newRoom = current.direction(step)
        val newElfMap = elfMap.connect(current, newRoom)
        return buildElfMap(re.tail, newElfMap, newRoom, branchPoints)
      } else {
        throw new Exception("unrecognized char in regex: " + step)
      }
    } else {
      throw new Exception("no more chars in regex but $ not encountered")
    }
  }

}
