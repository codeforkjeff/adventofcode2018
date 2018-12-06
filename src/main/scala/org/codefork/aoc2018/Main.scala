package org.codefork.aoc2018

object Main {

  val parts: Seq[Part] = Seq(
    Day1Part1,
    Day1Part2,
    Day2Part1,
    Day2Part2,
    Day3Part1,
    Day3Part2,
    Day4Part1,
    Day4Part2,
    Day5Part1,
    Day5Part2
  )

  def getClassName(o: Object) = {
    o.getClass.getSimpleName.replace("$", "")
  }

  def main(args: Array[String]) = {
    val filtered =
      if (args.nonEmpty) { parts.filter(p => args.contains(getClassName(p))) } else
        parts
    filtered.foreach(part => {
      val name = getClassName(part)
      val t0 = System.currentTimeMillis()
      val answer = part.answer
      val t1 = System.currentTimeMillis()
      println(s"$name Answer: $answer")
      println("  Elapsed time: " + (t1 - t0) + "ms")
    })
  }

}
