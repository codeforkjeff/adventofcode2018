package org.codefork.aoc2018

import org.codefork.aoc2018.Day16.Registers

/**
 * I think there's two broad approaches to this problem:
 *
 * 1. Analyze the loops in the program, try to figure out what they effectively
 * calculate when they end, and short-circuit them. Trace the program using pen and paper
 * and calculate the final answer. As far as I could tell, all the comments on the reddit
 * thread took this approach to get the answer:
 *
 * https://www.reddit.com/r/adventofcode/comments/a7j9zc/2018_day_19_solutions/
 *
 * 2. Write some sort of profiler/optimizer that does the above programmatically.
 * This is WAAY harder but also way more interesting. I started down the route
 * of doing loop detection and recording the net effect on the registers, but
 * this can't tell you what the calculation actually is (in this case, a modulus operation?).
 * I'm not sure it's even possible to write an optimizer of this sort.
 *
 * So I gave up. It was vaguely interesting to think about how this machine works in part 1
 * but I wasn't up to the analysis in part 2, and I was a bit disappointed that there's no
 * real code solution for it.
 */
object Day19Part2 extends Part {

  override def answer: String = {
    //val result = Day19.getProgram(Registers(Seq(1,0,0,0,0,0))).execute
    //result.registers.get(0).toString
    ""
  }

}
