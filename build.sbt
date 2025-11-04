name := "eis-assignments"

version := "dev"

scalaVersion := "3.3.7"
scalaBinaryVersion := "3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test
)

Compile / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/report")
