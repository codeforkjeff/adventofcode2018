
# Advent of Code 2018

See https://adventofcode.com/2018

These are my solutions, written in Scala, using IntelliJ. Updated Nov 2022 to use Scala 3.2.0.

## Post-Mortem

I did most of the puzzles during Dec 2018 and was randomly inspired to
try to finish them in Dec 2022. I'm going to say I'm done with this
now.

Here's what's left unfinished:

- Day 15 Part 2 - uses a brute force method that takes ~140s to run, needs
  optimization but I was too lazy to return to it.

- Day 19 Part 2 - I'm not sure there's a solution in code for
  this. Everyone on reddit seemed to solve it by noticing what the
  loops effectively do and calculating the final answer using pen and
  paper.

- Day 21 - same type of problem as Day 19 Part 2.

- Day 25 Part 2 - you only get the input if you finish all the other puzzles first.

Reflections:

- Overall, I really enjoyed AoC and appreciate that its creator works
  on it every year. It was extremely challenging, a good way for me to
  get better at Scala, a fun way to revisit my knowledge of algorithms
  and data structures, and a nice departure from the kind of coding I
  do for work.

- Unless you're a genius, plan to spend A LOT OF TIME on these puzzles
  if you really want to finish them all. 2018 was the only year where
  I had a December with enough free time to make a serious attempt at
  completing AoC. And I didn't.
  
- You can always return to the unfinished puzzles! The website pages
  for past years still work: you can still submit answers. I found it
  more enjoyable to take my time and strive for elegant or performant
  solutions, rather than race for a position on the leaderboard, which
  is out of my league frankly.
  
- Things I learned about Scala: case classes are the key to bridging
  OOP and FP; writing iterative code using recursion gets easier over
  time; immutability forces you to constantly think of all code as
  data transformations, which I find satisfying.

- Looking at the `/r/adventofcode` reddit feels like cheating. And
  yes, it is. But in retrospect, I wish I had limited the time I spent
  on each puzzle to 2-3 days. There's no point to tearing your hair
  out for a week on a puzzle. And I still learned a lot from
  implementing the algorithms and strategies that other people came up
  with. The code is still my own.

## Running it

You can open this folder in IntelliJ and run each part individually
from the IDE (open the file, right-click in the editor, select "Run
[name]")

You can also run the code from the command line. Install
[sbt](https://www.scala-sbt.org). From the top directory of this repo,
build the project by running:

```
sbt assembly
```

To generate and display the solutions (including elapsed time):

```sh
# all the solutions
java -jar target/scala-3.2.0/adventofcode2018-assembly-0.1.0-SNAPSHOT.jar

# just the solution for Day5Part1
java -jar target/scala-3.2.0/adventofcode2018-assembly-0.1.0-SNAPSHOT.jar Day5Part1

# multiple solutions
java -jar target/scala-3.2.0/adventofcode2018-assembly-0.1.0-SNAPSHOT.jar Day5Part1 Day5Part2
```
