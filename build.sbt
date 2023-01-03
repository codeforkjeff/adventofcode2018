val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode2018",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    assembly / mainClass := Some("org.codefork.aoc2018.Main")
  )

scalacOptions ++= Seq("-deprecation", "-feature")
