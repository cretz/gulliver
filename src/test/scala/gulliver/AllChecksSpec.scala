package gulliver

import java.io.{PrintStream, ByteArrayOutputStream, File}
import java.lang.reflect.Modifier
import gulliver.parse.Parser
import gulliver.compile.Compiler
import gulliver.util.Classpath
import org.scalatest._
import scala.io.Source
import gulliver.parse.Ast
import scala.util.Success
import gulliver.spec.SpecEntry

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
//      val ret = files.filter(_.getName.endsWith(".swift")) ++
//        files.filter(_.isDirectory).flatMap(getSpecFiles _)
//      ret.filterNot(_.getAbsolutePath.contains("swift\\"))
    }
    println(getSpecFiles().toSeq)
    getSpecFiles().foreach { f =>
      it should "validate " + f.getAbsolutePath in {
        // Load file and get path relative to spec folder
        val code = {
          val src = Source.fromFile(f)
          try { src.mkString } finally { src.close() }
        }
        val relativePath = f.getAbsolutePath.replace(specDir.getAbsolutePath, "").
          replace('\\', '/').dropWhile('/'==_)
        // Parse it
        val decls = Parser.parse(Parser.Settings(Map(relativePath -> code))).mapValues(_.get)
        // Get the spec entry info
        val spec = SpecEntry(code)
        // Compile it
        val Left(result) = Compiler.compile(Compiler.Settings(Classpath.Default, decls))
        // Create class loader
        val loader = new TestClassLoader
        // Define classes, getting back the static main methods
        val mainMethods = result.flatMap { case (fileName, bytes) =>
          val className = fileName.dropRight(".class".length).replace('/', '.')
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
          mainMethods.head.invoke(null, Array.empty[String])
        } finally { System.setOut(previousOut) }
        // Get output string without carriage retruns
        val actualOutput = new String(stdout.toByteArray).replace("\r", "")
        // Validate output (remove carriage returns from
        Some(actualOutput) should be(spec.output)
      }
    }
  }
}
