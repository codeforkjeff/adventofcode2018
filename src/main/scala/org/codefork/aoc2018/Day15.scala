package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day15 {

  case class XY(x: Int, y: Int)

  sealed trait Thing {
    def xy: XY
    def isPerson: Boolean
    def isOpenSpace: Boolean
  }

  // 'Unit' is a reserved word in scala so we use Person
  case class Person(xy: XY, personType: Char, id: String, hp: Int = 200)
      extends Thing {
    def isPerson = true
    def isOpenSpace = false

    override def toString: String = personType.toString
  }

  case class Wall(xy: XY) extends Thing {
    def isPerson = false
    def isOpenSpace = false
    override def toString: String = "#"
  }

  case class Empty(xy: XY) extends Thing {
    def isPerson = false
    def isOpenSpace = true
    override def toString: String = "."
  }

  // 'reading order' means we should store coordinates as Seq
  case class Battlefield(field: Map[XY, Thing] = Map.empty,
                         roundsCompleted: Int = 0,
                         finished: Boolean = false) {

    val people: Seq[Person] = field.keys.toSeq
      .filter(
        xy =>
          field(xy) match {
            case Person(_, _, _, _) => true
            case _                  => false
        }
      )
      .sortBy(xy => (xy.y, xy.x))
      .map(xy => field(xy))
      .collect {
        case p: Person => p
      }

    val elves = people.filter(_.personType == 'E')

    val goblins = people.filter(_.personType == 'G')

    def display() = {
      println("Round " + roundsCompleted)
      val allY = field.keys.toSeq.groupBy(xy => xy.y)
      allY.keys.toSeq.sorted.foreach(y => {
        val allX = allY(y)
        val row = allX.sortBy(xy => xy.x).foldLeft(("", "")) { (acc, xy) =>
          {
            val hp = if (field(xy).isPerson) {
              field(xy).asInstanceOf[Person].personType + "(" + field(xy)
                .asInstanceOf[Person]
                .hp + ")"
            } else ""
            (acc._1 + field(xy).toString, acc._2 + hp)
          }
        }
        println(row._1 + " " + row._2)
      })
    }

    def fightToTheDeath(elfAttackPower: Int = 3): Battlefield = {
      //display()
      if (finished) {
        this
      } else {
        doRound(people, elfAttackPower).fightToTheDeath(elfAttackPower)
      }
    }

    def outcome = roundsCompleted * people.map(_.hp).sum

    @tailrec
    final def doRound(queue: Seq[Person], elfAttackPower: Int): Battlefield = {
      if (queue.size == 0) {
        copy(roundsCompleted = roundsCompleted + 1)
      } else if (elves.isEmpty || goblins.isEmpty) {
        //println("finished mid round")
        copy(finished = true)
      } else {
        val nextPerson = queue.head
        val personOpt = people.find(_.id == nextPerson.id)
        if (personOpt.isDefined) {
          val person = personOpt.get
          val (battlefieldAfterMove, moved) = move(person)
          val newBattlefield =
            battlefieldAfterMove.attack(moved, elfAttackPower)
          newBattlefield.doRound(queue.tail, elfAttackPower)
        } else {
          // this happens when a person dies during a round before it's their turn
          doRound(queue.tail, elfAttackPower)
        }
      }
    }

    def move(person: Person): (Battlefield, Person) = {
      val xyOpt =
        if (adjacentEnemies(person).isEmpty)
          findSquareToMoveTo(person)
        else None

      val newPerson = if (xyOpt.isDefined) {
        //println("moving " + person.xy + " to " + xyOpt.get)
        person.copy(xy = xyOpt.get)
      } else person

      if (xyOpt.isDefined) {
        val replace: Map[XY, Thing] =
          Map(xyOpt.get -> newPerson, person.xy -> Empty(person.xy))
        (copy(field ++ replace), newPerson)
      } else (this, person)
    }

    def attack(person: Person, elfAttackPower: Int = 3): Battlefield = {
      val enemies = adjacentEnemies(person)
      if (enemies.nonEmpty) {
        val target = enemies.minBy(e => (e.hp, e.xy.y, e.xy.x))
        val attackPower = if (person.personType == 'E') elfAttackPower else 3
        val attacked = target.copy(hp = target.hp - attackPower)
        val replace = if (attacked.hp <= 0) Empty(attacked.xy) else attacked
        copy(field + (attacked.xy -> replace))
      } else this
    }

    def findSquareToMoveTo(person: Person): Option[XY] = {

      val destinations = openAdjacent(person.xy).toSet

      val targets = if (person.personType == 'E') goblins else elves

      // for each target, get squares "in range" and determine shortest paths from those squares
      // to person
      // TODO: this is super slow
      val shortestPaths =
        targets
        // 'in range' = open sq adjacent to targets
          .flatMap(target => openAdjacent(target.xy))
          .map(
            inRange => {
              //println("finding shortest path for " + inRange)
              ShortestPath(this,
                           inRange,
                           destinations,
                           0,
                           Map(0 -> Set(inRange))).find
            })
          .filter(_.isDefined)
          .map(_.get)

      if (shortestPaths.nonEmpty)
        Some(
          shortestPaths
            .minBy(
              result =>
                (result.d,
                 result.targetXy.y,
                 result.targetXy.x,
                 result.destXy.y,
                 result.destXy.x))
            .destXy)
      else
        None
    }

    def openAdjacent(xy: XY) = {
      Seq(XY(xy.x - 1, xy.y),
          XY(xy.x, xy.y - 1),
          XY(xy.x + 1, xy.y),
          XY(xy.x, xy.y + 1)).filter(xy => field(xy).isOpenSpace)
    }

    def adjacentEnemies(person: Person): Seq[Person] = {
      val xy = person.xy
      val enemy = if (person.personType == 'E') 'G' else 'E'
      Seq(XY(xy.x - 1, xy.y),
          XY(xy.x, xy.y - 1),
          XY(xy.x + 1, xy.y),
          XY(xy.x, xy.y + 1))
        .map(field(_))
        .collect {
          case p: Person => p
        }
        .filter(p => p.personType == enemy)
    }
  }

  case class PathResult(targetXy: XY, destXy: XY, d: Int)

  // find the shortest path from inRange square to one of destination squares
  case class ShortestPath(battlefield: Battlefield,
                          inRangeXy: XY,
                          destinations: Set[XY],
                          d: Int = 0,
                          distances: Map[Int, Set[XY]] = Map.empty) {

    @tailrec
    final def find: Option[PathResult] = {
      val covered = distances.values.flatten.toSet
      val intersect = destinations.intersect(covered)
      if (intersect.nonEmpty) {
        Some(PathResult(inRangeXy, intersect.minBy(xy => (xy.y, xy.x)), d))
      } else {
        val adjToLastD =
          distances(d).flatMap(xy => battlefield.openAdjacent(xy))
        val newDistances = adjToLastD.diff(covered)
        if (newDistances.nonEmpty)
          copy(d = d + 1, distances = distances + ((d + 1) -> newDistances)).find
        else
          None
      }
    }

  }

  def getInput = {
    val url = getClass.getResource("/day15/input.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def getTestData1 = {
    val url = getClass.getResource("/day15/testData1.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def getTestData2 = {
    val url = getClass.getResource("/day15/testData2.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def getTestData3 = {
    val url = getClass.getResource("/day15/testData3.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def getTestData4 = {
    val url = getClass.getResource("/day15/testData4.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def getTestData5 = {
    val url = getClass.getResource("/day15/testData5.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def getTestData6 = {
    val url = getClass.getResource("/day15/testData6.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def buildBattlefield(lines: Seq[String], elfAttackPower: Int = 3) = {
    val field = lines.zipWithIndex.foldLeft(Map[XY, Thing]()) {
      case (acc, (line, y)) => {
        line.zipWithIndex.filter(_._1 != ' ').foldLeft(acc) {
          case (acc, (ch, x)) => {
            val xy = XY(x, y)

            val thing: Thing =
              if (ch == 'E' || ch == 'G')
                Person(xy, ch, xy.toString)
              else if (ch == '.')
                Empty(xy)
              else
                Wall(xy)

            acc + (xy -> thing)
          }
        }
      }
    }
    Battlefield(field)
  }
}
