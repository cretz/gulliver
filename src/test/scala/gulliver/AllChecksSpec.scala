package gulliver

import java.io.{PrintStream, ByteArrayOutputStream, File}
import java.lang.reflect.Modifier

import gulliver.parse.Parser
import gulliver.compile.Compiler
import gulliver.util.Classpath
import org.scalatest._

import scala.io.Source

class AllChecksSpec extends GulliverSpec {
  behavior of "Gulliver language suite"

  class TestClassLoader extends ClassLoader {
    def defineClass(name: String, b: Array[Byte]): Class[_] = defineClass(name, b, 0, b.length)
  }

  val specDir = new File("spec")

  if (!specDir.isDirectory) ignore should "handle specification checks" in { }
  else {
    // Recursively obtain all swift files
    def getSpecFiles(dir: File = specDir): Array[File] = {
      require(dir.isDirectory)
      val files = dir.listFiles
      files.filter(_.getName.endsWith(".swift")) ++ files.filter(_.isDirectory).flatMap(getSpecFiles _)
    }
    getSpecFiles().foreach { f =>
      it should "validate " + f.getName in {
        // Load file and get path relative to spec folder
        val code = {
          val src = Source.fromFile(f)
          try { src.mkString } finally { src.close() }
        }
        val relativePath = f.getAbsolutePath.replace(specDir.getAbsolutePath, "").replace('\\', '/')
        // Parse it
        val decls = Parser.parse(Parser.Settings(Map(relativePath -> code))).mapValues(_.get)
        // Compile it
        val results = Compiler.compile(Compiler.Settings(Classpath.Default, decls))
        // Check errors then get classes
        results.values.foreach(res => require(res.errors.isEmpty, sys.error("Err: " + res.errors)))
        val classes = results.values.flatMap(_.classes).toMap
        // Create class loader
        val loader = new TestClassLoader
        // Define classes, getting back the static main methods
        val mainMethods = classes.flatMap { case (className, bytes) =>
          val cls = loader.defineClass(className, bytes)
          try {
            val method = cls.getDeclaredMethod("main", classOf[Array[String]])
            if (Modifier.isStatic(method.getModifiers)) Some(method) else None
          } catch { case _: NoSuchMethodException => None }
        }
        require(mainMethods.size == 1)
        // Wrap stdout and run
        val stdout = new ByteArrayOutputStream
        val previousOut = System.out
        try {
          System.setOut(new PrintStream(stdout))
          mainMethods.head.invoke(null)
        } finally { System.setOut(previousOut) }
        // Load up check and validate
        val check = {
          val src = Source.fromFile(f.getAbsolutePath.dropRight(6) + ".check")
          try { src.mkString } finally { src.close() }
        }
        new String(stdout.toByteArray) should be(check)
      }
    }
  }
}
