package org.codefork.aoc2018

sealed trait Status
case object Finished extends Status
case object NeedsOptimization extends Status
case object Incomplete extends Status

trait Part {

  def getClassName =
    getClass.getSimpleName.replace("$", "")

  def status: Status = Finished

  val validateTestData = true

  // default to noop
  def assertTestCases(): Unit = ()

  def answer: String

  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    val name = getClassName
    if(validateTestData) {
      assertTestCases()
    }
    val t0 = System.currentTimeMillis()
    val answerValue = answer
    val t1 = System.currentTimeMillis()
    val elapsed = t1 - t0
    println(f"$name Answer: $answerValue%-40s Time: $elapsed ms")
  }

}
