name := "gulliver"

version := "0.1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.1"

libraryDependencies += "org.ow2.asm" % "asm" % "5.0.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

testOptions in Test += Tests.Argument("-oF")