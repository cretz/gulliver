name := "gulliver"

version := "0.1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.1"

libraryDependencies += "org.ow2.asm" % "asm" % "5.0.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

fullClasspath in Test += Attributed.blank(file(System.getProperty("java.home").dropRight(3) + "lib/tools.jar"))

testOptions in Test += Tests.Argument("-oF")