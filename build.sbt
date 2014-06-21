name := "gulliver"

version := "1.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.0-RC2"

libraryDependencies += "org.ow2.asm" % "asm" % "5.0.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

testOptions in Test += Tests.Argument("-oF")