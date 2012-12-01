name := "CanCan"

version := "1.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

initialCommands in console := "import cancan._"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"
