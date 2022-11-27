
# Advent of Code 2018

See https://adventofcode.com/2018

These are my solutions, written in Scala, using IntelliJ. Updated Nov 2022 to use Scala 3.2.0.

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
