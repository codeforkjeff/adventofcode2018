
# Advent of Code 2018

See https://adventofcode.com

These are my solutions, written in Scala, using IntelliJ.

## Running it

You can open this folder in IntelliJ and run each part individually
from the IDE (open the file, right-click in the editor, select "Run
[name]")

You can also run the code from the command line. Install
[sbt](https://www.scala-sbt.org). From the top directory of this repo,
build the project by running:

```
sbt stage
```

To generate and display the solutions (including elapsed time):

```sh
# all the solutions
./target/universal/stage/bin/main

# just the solution for Day5Part1
./target/universal/stage/bin/main Day5Part1

# multiple solutions
./target/universal/stage/bin/main Day5Part1 Day5Part2
```
