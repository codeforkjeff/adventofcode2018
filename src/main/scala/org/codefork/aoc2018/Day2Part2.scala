package org.codefork.aoc2018

import scala.io.Source

object Day2Part2 {

  // return the common chars at each index between id1 and id2
  def commonChars(id1: String, id2: String): String =
    0.to(id1.length - 1)
      .map(pos =>
        if (id1.charAt(pos) == id2.charAt(pos)) id1.charAt(pos) else "")
      .mkString("")

  // search in remainder for item that is diff by 1 char from id,
  // returning the chars they have in common
  def search(id: String, remainder: Seq[String]): Option[String] = {
    if(remainder.nonEmpty) {
      val id2 = remainder.head
      val common = commonChars(id, id2)
      // diff by 1 char?
      if (common.length == id.length - 1) {
        Some(common)
      } else {
        search(id, remainder.tail)
      }
    } else {
      None
    }
  }

  // exhaustive search comparing every id to every other id
  def searchAll(id: String, remainder: Seq[String]): String = {
    val result = search(id, remainder)
    if(result.isEmpty) {
      searchAll(remainder.head, remainder.tail)
    } else {
      result.get
    }
  }

  def main(args: Array[String]): Unit = {
    val url = getClass.getResource("/day2/input.txt")
    val common = searchAll("", Source.fromURL(url).getLines().toSeq)
    println(common)
  }

}
