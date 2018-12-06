package org.codefork.aoc2018

object Part {
  def getClassName(o: Object) =
    o.getClass.getSimpleName.replace("$", "")
}

trait Part {

  def answer: String

  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    val name = Part.getClassName(this)
    val t0 = System.currentTimeMillis()
    val answerValue = answer
    val t1 = System.currentTimeMillis()
    val elapsed = t1 - t0
    println(f"$name Answer: $answerValue%-40s Time: $elapsed ms")
  }

}
