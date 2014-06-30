package gulliver.tools

import java.io.File

import gulliver.util.Classpath

object Gulliverc extends App {
  case class Settings(
    classpath: Classpath,
    inDir: File,
    outDir: File = new File("."))

  def run(settings: Settings): Unit = ???
}
