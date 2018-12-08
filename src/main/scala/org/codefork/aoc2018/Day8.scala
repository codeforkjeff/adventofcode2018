package org.codefork.aoc2018

import scala.collection.immutable.Queue
import scala.io.Source

object Day8 {

  case class Node(numChildren: Int,
                  numMetadata: Int,
                  children: Queue[Node] = Queue.empty,
                  metadata: List[Int] = List.empty) {

    def addChild(child: Node) =
      copy(children = (children :+ child))

    def setMetadata(metadata: List[Int]) =
      copy(metadata = metadata)

  }

  def makeTree(input: List[Int], stack: List[Node] = List[Node]()): Node = {
    // are we done making children?
    if (stack.nonEmpty) {
      val node = stack.head
      if (node.numChildren == node.children.size) {
        val newNode = node.setMetadata(input.take(node.numMetadata))

        return if (stack.tail.nonEmpty) {
          // add child to parent if there is one
          val parent = stack.tail.head
          val newParent = parent.addChild(newNode)

          val remainingInput = input.drop(node.numMetadata)

          makeTree(remainingInput, newParent +: stack.tail.tail)
        } else {
          // no parent means we've depleted stack; we're done.
          newNode
        }
      }
    }

    // create node with partial info, push it on stack, recurse
    val numChildren = input.head
    val numMetadata = input.tail.head
    val node = Node(numChildren, numMetadata)

    val remaining = input.tail.tail

    makeTree(remaining, node +: stack)
  }

  def tallyMetadata(remaining: Seq[Node], runningTally: Int = 0): Int =
    if (remaining.size == 0) runningTally
    else
      tallyMetadata(remaining.flatMap(n => n.children),
        runningTally + remaining.foldLeft(0) { (acc, node) =>
        {
          acc + node.metadata.sum
        }
        })

  def getTestData = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

  def getInputData = {
    val url = getClass.getResource("/day8/input.txt")
    Source.fromURL(url).mkString.stripLineEnd
  }

  def inputToList(s: String) =
    s.split(" ").map(_.toInt).toList

}
