package org.codefork.aoc2018

object Day18Part2 extends Part {

  override def answer: String = {
    val initial = Day18.getInput.populateAdjacentCache()

    // another goofy "solution" that's just happenstance of noticing a pattern by examination.
    // starting at minute 577, a pattern repeats every 28 mins.

    val (result, minToResourceValue) =
      1.to(605).foldLeft((initial, Map[Int, Int]())) { (acc, i) =>
        val newLand = acc._1.transform
        val minToResourceValue = acc._2
        (newLand, minToResourceValue + (i -> newLand.resourceValue))
      }

    val modulusLookup = minToResourceValue
      .filter { case (min, _) => min >= 577 }
      .map {
        case (min, value) => (min - 577 -> value)
      }

    val huge: BigInt = BigInt("1000000000")
    val mod = (huge - 577) % 28
    modulusLookup(mod.toInt).toString
  }

}
