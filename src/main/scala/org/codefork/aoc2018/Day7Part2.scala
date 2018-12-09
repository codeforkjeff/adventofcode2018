package org.codefork.aoc2018

import scala.annotation.tailrec

object Day7Part2 extends Part {

  case class Task(step: Char, startTime: Int, baseStepTime: Int) {
    def availableAt: Int = startTime + baseStepTime + (step.toInt - 64)
  }

  case class Worker(tasks: Seq[Task] = Seq.empty)

  case class Build(steps: String,
                   workers: Seq[Worker],
                   deps: Map[Char, String],
                   prereqs: Map[Char, String],
                   finished: Map[Char, Int],
                   baseStepTime: Int) {

    def getEarliestStartTime(step: Char) =
      if (!prereqs.contains(step)) 0
      else
        prereqs(step).map(prereq => finished(prereq)).max

    def assignToAvailableWorker(step: Char): (Task, Seq[Worker]) = {
      val earliestStartTime = getEarliestStartTime(step)
      val (startTime, i) =
        if (workers.forall(_.tasks.size == 0)) (0, 0)
        else {
          // if worker is elig to run the step, when would they start? pick the earliest
          workers.zipWithIndex
            .map {
              case (w, i) => {
                val startTime =
                  if (w.tasks.isEmpty) {
                    earliestStartTime
                  } else if (w.tasks.last.availableAt > earliestStartTime) {
                    w.tasks.last.availableAt
                  } else {
                    earliestStartTime
                  }
                (startTime, i)
              }
            }
            .filter { case (value, i) => value >= earliestStartTime }
            .sortBy(_._1)
            .min
        }
      val worker = workers(i)
      val task = Task(step, startTime, baseStepTime)
      val newWorker = worker.copy(tasks = (worker.tasks :+ task))
      (task, workers.patch(i, Seq(newWorker), 1))
    }

    @tailrec
    final def calculateTime: Int = {
      if (steps.size == 0) {
        workers
          .map(w => if (w.tasks.isEmpty) 0 else w.tasks.last.availableAt)
          .max
      } else {
        // sort by their required start time, then alphabetically, to ensure
        // we do steps in order in which they become eligible to be performed
        val withReqStartTimes = steps
          .map(c => (c, getEarliestStartTime(c)))
          .sortBy(c => (c._2, c._1))
        val sorted = withReqStartTimes.map(c => c._1).mkString

        // select a step
        val step = sorted.head
        val remainingSteps = sorted.tail

        // assign the step to a worker
        val (task, newWorkers) = assignToAvailableWorker(step)

        val newFinished = finished + (step -> task.availableAt)

        // children become steps to be run only when ALL their prereqs are fulfilled
        val children = deps.getOrElse(step, "")
        val moreSteps = children.filter(c =>
          prereqs(c).forall(prereq => newFinished.contains(prereq)))
        val newSteps = remainingSteps + moreSteps

        copy(steps = newSteps, workers = newWorkers, finished = newFinished).calculateTime
      }
    }

  }

  def runTestData: String = {
    val workers = Seq.fill(2) { Worker() }
    val time = Build("C",
                     workers,
                     Day7.testDataDeps,
                     Day7.testDataPrereqs,
                     Map.empty,
                     0).calculateTime
    time.toString
  }

  def runInputData: String = {
    val deps = Day7.getDependencies
    val initial = deps.keys.toSet.diff(deps.values.flatten.toSet).mkString
    val workers = Seq.fill(5) { Worker() }
    val time =
      Build(initial, workers, deps, Day7.getPrereqs, Map.empty, 60).calculateTime
    time.toString
  }

  override def answer: String = {
    //runTestData
    runInputData
  }

}
