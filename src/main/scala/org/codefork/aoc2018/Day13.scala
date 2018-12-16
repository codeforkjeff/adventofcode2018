package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day13 {

  case class XY(x: Int, y: Int) {
    def left = copy(x = x - 1)
    def right = copy(x = x + 1)
    def up = copy(y = y - 1)
    def down = copy(y = y + 1)
  }

  type Direction = Char

  trait TrackSegment {
    def xy: XY

    // given a cart that is entering INTO this segment, return its new direction
    def changeCartDirection(cart: Cart): Direction

    // update cart's nextTurn: default impl does nothing (only intersections should do this)
    def updateNextTurn(cart: Cart): Direction = cart.nextTurn
  }

  case class Straight(xy: XY, outlets: Set[XY]) extends TrackSegment {

    def changeCartDirection(cart: Cart) = {
      if (cart.trackSegment.xy.x != xy.x) {
        if (cart.trackSegment.xy.x - xy.x > 0) {
          '<'
        } else {
          '>'
        }
      } else {
        if (cart.trackSegment.xy.y - xy.y > 0) {
          '^'
        } else {
          'v'
        }
      }
    }
  }

  case class Curve(xy: XY, outlets: Set[XY]) extends TrackSegment {

    def changeCartDirection(cart: Cart) = {
      // figure out exit
      val exit = (outlets - cart.trackSegment.xy).head
      if (exit.x > xy.x) {
        '>'
      } else if (exit.x < xy.x) {
        '<'
      } else if (exit.y > xy.y) {
        'v'
      } else {
        '^'
      }
    }
  }

  case class Intersection(xy: XY) extends TrackSegment {

    def changeCartDirection(cart: Cart) = {
      val xChange = xy.x - cart.trackSegment.xy.x
      val yChange = xy.y - cart.trackSegment.xy.y

      if (xChange > 0) {
        // moving right
        cart.nextTurn match {
          case '<' => '^'
          case '|' => '>'
          case '>' => 'v'
        }
      } else if (xChange < 0) {
        // moving left
        cart.nextTurn match {
          case '<' => 'v'
          case '|' => '<'
          case '>' => '^'
        }
      } else if (yChange > 0) {
        // moving down
        cart.nextTurn match {
          case '<' => '>'
          case '|' => 'v'
          case '>' => '<'
        }
      } else {
        // moving up
        cart.nextTurn match {
          case '<' => '<'
          case '|' => '^'
          case '>' => '>'
        }
      }
    }

    override def updateNextTurn(cart: Cart): Direction = {
      cart.nextTurn match {
        case '<' => '|'
        case '|' => '>'
        case '>' => '<'
      }
    }

  }

  case class Cart(trackSegment: TrackSegment,
                  headed: Direction,
                  nextTurn: Direction = '<')

  case class System(segments: Map[XY, TrackSegment] = Map.empty,
                    carts: Set[Cart] = Set.empty,
                    collisions: Seq[XY] = Seq.empty) {

    def moveCarts = {
      val newSystem = carts.toSeq
        .sortBy(c => (c.trackSegment.xy.y, c.trackSegment.xy.x))
        .foldLeft(this) { (acc, cart) =>
          {
            val nextXy = cart.headed match {
              case '<' => cart.trackSegment.xy.left
              case '^' => cart.trackSegment.xy.up
              case '>' => cart.trackSegment.xy.right
              case 'v' => cart.trackSegment.xy.down
            }
            val nextSegment = segments(nextXy)
            val newDir = nextSegment.changeCartDirection(cart)
            val newNextTurn = nextSegment.updateNextTurn(cart)
            val newCart = cart.copy(nextSegment, newDir, newNextTurn)

            // update any collisions that happen after a single cart moves
            val updatedCarts = (acc.carts - cart) + newCart
            val collision = getCollision(updatedCarts)
            val updatedCollisions = acc.collisions ++
              (if(collision.isDefined) Seq(collision.get) else Seq.empty)

            acc.copy(carts = updatedCarts, collisions = updatedCollisions)
          }
        }
      newSystem
    }

    def getCollision(carts: Set[Cart]): Option[XY]  = {
      val headOpt = carts
        .groupBy(_.trackSegment.xy)
        .find { case (_, grouped) => grouped.size > 1 }
      if (headOpt.isDefined)
        Some(headOpt.get._1)
      else
        None
    }

    @tailrec
    final def findFirstCollision: XY = {
      val newSystem = moveCarts
      if(newSystem.collisions.nonEmpty)
        newSystem.collisions.head
      else
        newSystem.findFirstCollision
    }

  }

  object System {

    def build(lines: Seq[String]): System = {

      // build grid first because we'll need to do lookups of neighbors when building segments
      val grid = lines.zipWithIndex
        .foldLeft(Map[XY, Char]()) {
          case (acc, (line, y)) => {
            acc ++ line.zipWithIndex
              .filter { case (ch, _) => ch != ' ' }
              .map { case (ch, x) => (XY(x, y) -> ch) }
              .toMap
          }
        }

      val system = grid.foldLeft(System()) {
        case (acc, (xy, ch)) => {

          val segment: TrackSegment = if (ch == '-' || ch == '>' || ch == '<') {
            Straight(xy, Set(xy.left, xy.right))
          } else if (ch == '|' || ch == '^' || ch == 'v') {
            Straight(xy, Set(xy.up, xy.down))
          } else if (ch == '\\') {
            val outlets =
              if (grid.getOrElse(xy.up, ' ') == '+' || grid.getOrElse(
                    xy.up,
                    ' ') == '|') {
                Set(xy.up, xy.right)
              } else {
                Set(xy.down, xy.left)
              }
            Curve(xy, outlets)
          } else if (ch == '/') {
            val outlets =
              if (grid.getOrElse(xy.up, ' ') == '+' || grid.getOrElse(
                    xy.up,
                    ' ') == '|') {
                Set(xy.up, xy.left)
              } else {
                Set(xy.down, xy.right)
              }
            Curve(xy, outlets)
          } else if (ch == '+') {
            Intersection(xy)
          } else {
            throw new Exception("unhandled char: " + ch)
          }

          val newCarts = if (ch == '>' || ch == '<' || ch == '^' || ch == 'v') {
            acc.carts + Cart(segment, ch)
          } else {
            acc.carts
          }

          acc.copy(acc.segments + (xy -> segment), newCarts)
        }
      }
      system
    }
  }

  def getInput = {
    val url = getClass.getResource("/day13/input.txt")
    Source.fromURL(url).getLines().toSeq
  }

  def getTestData = {
    val url = getClass.getResource("/day13/testData.txt")
    Source.fromURL(url).getLines().toSeq
  }
}
