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

  case class ShortestPath(prev: Map[Room, Room], lastRoom: Room) {

    final def calcLevel: Int = calcLevel(lastRoom, 0)

    @tailrec
    final def calcLevel(room: Room, level: Int): Int = {
      if (prev(room) == Room(0, 0)) {
        level + 1
      } else {
        calcLevel(prev(room), level + 1)
      }
    }
  }

  case class ElfMap(doors: Map[Room, List[Room]]) {
    def connect(room1: Room, room2: Room): ElfMap = {
      val rooms = doors.getOrElse(room1, List.empty[Room])
      val roomsRev = doors.getOrElse(room2, List.empty[Room])
      // in both directions
      copy(doors ++ Map(
        room1 -> (rooms ++ List(room2)),
        room2 -> (roomsRev ++ List(room1))
      ))
    }

    def getShortestPath: ShortestPath = getShortestPath(Queue(Room(0,0)), Set(Room(0,0)), Map.empty, List.empty)

    @tailrec
    final def getShortestPath(queue: Queue[Room], visited: Set[Room], prev: Map[Room, Room], lastConnected: List[Room]): ShortestPath = {
      if(queue.nonEmpty) {
        val (v, remainder) = queue.dequeue
        val connected = doors.getOrElse(v, List.empty).filter(!visited.contains(_))
        if (connected.length > 0) {
          val newQueue = queue.enqueue(connected)
          val newPrev = prev ++ connected.map(_ -> v).toMap
          val newVisited = visited ++ connected
          return getShortestPath(newQueue, newVisited, newPrev, connected)
        } else {
          return getShortestPath(remainder, visited, prev, lastConnected)
        }
      } else {
        return ShortestPath(prev, lastConnected.head)
      }
    }
  }

  def findFarthestDistance(re: String) = {
    val elfMap = processRegex(re)
    val shortestPath = elfMap.getShortestPath
    shortestPath.calcLevel
  }

  def countShortestDistancesAbove(re: String, threshold: Int) = {
    val elfMap = processRegex(re)
    val shortestPath = elfMap.getShortestPath
    val candidates = shortestPath.prev.keys.filter(_ != Room(0,0))
    candidates.count(c => {
      shortestPath.calcLevel(c, 0) >= threshold
    })
  }

  def processRegex(re: String): ElfMap = {
    val current = Room(0,0)
    processRegex(re, ElfMap(Map(current -> List.empty)), current, List.empty)
  }

  @tailrec
  def processRegex(re: String, elfMap: ElfMap, current: Room, branchPoints: List[Room]): ElfMap = {
    if(re.headOption.nonEmpty) {
      val step = re.head
      if(step == '^') {
        return processRegex(re.tail, elfMap, current, branchPoints)
      } else if(step == '(') {
        val newBranchPoints = current +: branchPoints
        return processRegex(re.tail, elfMap, current, newBranchPoints)
      } else if(step == '|') {
        val lastBranchPoint = branchPoints.head
        return processRegex(re.tail, elfMap, lastBranchPoint, branchPoints)
      } else if(step == ')') {
        val lastBranchPoint = branchPoints.head
        val newBranchPoints = branchPoints.tail
        return processRegex(re.tail, elfMap, lastBranchPoint, newBranchPoints)
      } else if(step == '$') {
        return elfMap
      } else if("NEWS".indexOf(step) > -1) {
        val newRoom = current.direction(step)
        val newElfMap = elfMap.connect(current, newRoom)
        return processRegex(re.tail, newElfMap, newRoom, branchPoints)
      } else {
        throw new Exception("unrecognized char in regex: " + step)
      }
    } else {
      throw new Exception("no more chars in regex but $ not encountered")
    }
  }

}
