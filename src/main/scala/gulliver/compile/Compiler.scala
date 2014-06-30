package gulliver.compile

import java.io.File

import gulliver.transform.Module

object Compiler {
  case class Settings(
    modules: Seq[Module],
    outDir: Option[File] = None)

  def compile(settings: Settings): Map[String, Array[Byte]] = ???
}
