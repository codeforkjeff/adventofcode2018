package org.codefork.aoc2018

object Util {

  // merge two maps, summing its int values
  def mergeMapsSummingValues[A](map1: Map[A, Int], map2: Map[A, Int]) =
    map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }

  // replace the last item in a Seq
  def replaceLast[A](s: Seq[A], replacement: A) = s.patch(s.size - 1, Seq(replacement), 1)

  def crossProduct[A](a: Seq[A], b: Seq[A]): Seq[(A,A)] =
    a.flatMap(x => b.map(y => (x, y)))

}
