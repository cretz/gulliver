package gulliver.util

import java.io.File

case class Classpath(entries: Seq[Classpath.Entry]) {

}

object Classpath {
  val JavaRuntimeEntry = EntryJar(new File(System.getProperty("sun.boot.class.path"), "rt.jar"))
  val Default = Classpath(Seq(JavaRuntimeEntry))

  trait Entry
  case class EntryJar(file: File) extends Entry
  case class EntryClassFile(file: File) extends Entry
  case class EntryDir(dir: File) extends Entry
  object Entry {
    // def apply()
  }
}