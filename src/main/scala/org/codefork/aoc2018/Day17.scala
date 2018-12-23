package org.codefork.aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day17 {

  case class XY(x: Int, y: Int) {

    def move(d: Direction) = d match {
      case Up    => copy(y = y - 1)
      case Down  => copy(y = y + 1)
      case Left  => copy(x = x - 1)
      case Right => copy(x = x + 1)
    }

  }

  val Clay = '#'
  val Sand = '.'
  val MovingWater = '|'
  val RestingWater = '~'

  def buildScan: Map[XY, Char] = {
    val url = getClass.getResource("/day17/input.txt")
    val lines = Source.fromURL(url).getLines().toSeq

    lines.foldLeft(Map[XY, Char]()) { (acc, line) =>
      {
        // tuple of all x and all y values for this line
        val allXY =
          line.split(",").map(_.trim).foldLeft((Seq[Int](), Seq[Int]())) {
            (xy, piece) =>
              {
                val axisAndRange = piece.split("=")
                val axis = axisAndRange(0)
                val range = axisAndRange(1)

                val values = if (range.contains(".")) {
                  val startAndEnd = range.split("\\.\\.")
                  startAndEnd(0).toInt.to(startAndEnd(1).toInt).toList
                } else {
                  Seq(range.toInt)
                }

                if (axis == "x") (values, xy._2) else (xy._1, values)
              }
          }

        val seqXY =
          Util.crossProduct(allXY._1, allXY._2).map(t => XY(t._1, t._2))
        val toAdd = seqXY.map(xy => (xy -> Clay)).toMap

        acc ++ toAdd
      }
    }

  }

  val source = XY(500, 0)

  sealed trait Direction
  object Up extends Direction
  object Down extends Direction
  object Left extends Direction
  object Right extends Direction

  def startFlow(scan: Map[XY, Char]): FlowState = {
    val minX = scan.keys.map(_.x).min
    val maxX = scan.keys.map(_.x).max
    val minY = scan.keys.map(_.y).min
    val maxY = scan.keys.map(_.y).max
    FlowState(scan, minX, maxX, minY, maxY, source).flow
  }

  // when water flows off a horizontal vein of clay in 2 directions, we queue one to sourcesToDo
  case class FlowState(scan: Map[XY, Char],
                       minX: Int,
                       maxX: Int,
                       minY: Int,
                       maxY: Int,
                       last: XY,
                       sourcesToDo: List[XY] = List.empty) {

    def soilAtXY(xy: XY): Char =
      scan.getOrElse(xy, Sand)

    def soilAtXY(x: Int, y: Int): Char =
      scan.getOrElse(XY(x, y), Sand)

    def display() = {
      minY
        .to(maxY)
        .foreach(y => {
          val row = minX.to(maxX).foldLeft("") { (acc, x) =>
            acc + soilAtXY(x, y).toString
          }
          println(row)
        })
    }

    // returns x values for row that is pooling with water on a 'ledge',
    // not including coordinates that extend beyond edge
    def fillHorizontal(xy: XY): Seq[Int] = {
      val leftX = 0
        .to(xy.x - 1)
        .reverse
        .takeWhile(x => {
          val beneath = soilAtXY(x, xy.y + 1)
          soilAtXY(x, xy.y) != Clay && (beneath == Clay || beneath == RestingWater)
        })
        .reduceOption(math.min)
        .getOrElse(xy.x)
      val rightX =
        (xy.x + 1)
          .to(maxX)
          .takeWhile(x => {
            val beneath = soilAtXY(x, xy.y + 1)
            soilAtXY(x, xy.y) != Clay && (beneath == Clay || beneath == RestingWater)
          })
          .reduceOption(math.max)
          .getOrElse(xy.x)

      leftX.to(rightX)
    }

    @tailrec
    final def flow: FlowState = {
      val xyToTest: XY = last.move(Down)
      val beneath = xyToTest.move(Down)

//      println("last=" + last + " xyToTest=" + xyToTest)
//      display()

      // did this flow it joins to already existing water flow?
      val joined = soilAtXY(xyToTest) == MovingWater || soilAtXY(xyToTest) == RestingWater

      if (xyToTest.y > maxY || joined) {
        if (sourcesToDo.nonEmpty) {
          //println("DOING NEXT SOURCE=" + sourcesToDo.head + " remaining sources = " + sourcesToDo.tail)
          copy(last = sourcesToDo.head, sourcesToDo = sourcesToDo.tail).flow
        } else {
          this
        }
      } else {
        val soilBeneath = soilAtXY(beneath)

        if (soilBeneath == Clay || soilBeneath == RestingWater) {
          // we hit clay or we're backing up vertically b/c water is pooling
          val xValues = fillHorizontal(xyToTest)

          val contentToLeft = soilAtXY(xValues.head - 1, xyToTest.y)
          val contentToRight = soilAtXY(xValues.last + 1, xyToTest.y)
          val atRest = contentToLeft == Clay && contentToRight == Clay
          val ch = if (atRest) RestingWater else MovingWater

          // extend by 1 on relevant sides if not atRest
          val xValuesForFill = if (!atRest) {
            val min =
              if (contentToLeft == Clay) xValues.min else xValues.min - 1
            val max =
              if (contentToRight == Clay) xValues.max else xValues.max + 1
            min.to(max)
          } else xValues

          val fill = xValuesForFill.map(x => (XY(x, xyToTest.y) -> ch))

          val newLast = if (!atRest) {
            // we always visit the left, if available; then right
            if (contentToLeft == Clay)
              XY(xValuesForFill.last, xyToTest.y)
            else
              XY(xValuesForFill.head, xyToTest.y)
          } else {
            last.move(Up)
          }

          val newSourcesTodo = if (!atRest) {
            // queue the path not taken
            val rightSide =
              if (contentToLeft != Clay && contentToRight != Clay)
                List(XY(xValuesForFill.last, xyToTest.y))
              else List.empty
            rightSide ++ sourcesToDo
          } else {
            sourcesToDo
          }

          // clear the moving water at 'last' because we're moving back up;
          // this is needed in order for the check for joining existing water to work
          val reset = if (atRest) {
            Map(last -> Sand)
          } else Map.empty

          copy(scan ++ fill ++ reset, last = newLast, sourcesToDo = newSourcesTodo).flow
        } else {
          // keep flowing down
          copy(scan + (xyToTest -> MovingWater), last = xyToTest).flow
        }
      }
    }

    def countWater =
      scan
        .filter { case (k, v) => k.y >= minY && k.y <= maxY }
        .values
        .count(ch => (ch == RestingWater) || (ch == MovingWater))

    def countRetained =
      scan
        .filter { case (k, v) => k.y >= minY && k.y <= maxY }
        .values
        .count(ch => (ch == RestingWater))

  }

}
