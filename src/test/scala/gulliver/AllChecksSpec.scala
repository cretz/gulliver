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
        val relativePath = f.getAbsolutePath.replace(specDir.getAbsolutePath, "").
          replace('\\', '/').dropWhile('/'==_)
        // Parse it
        val decls = Parser.parse(Parser.Settings(Map(relativePath -> code))).mapValues(_.get)
        // Parse the multiline comment from the top and remove the top line and all
        //  carriage returns
        val Success(Ast.MultilineComment(comment)) = new Parser(code).multilineComment.run()
        val cleanComment = comment.replace("\r", "")
        require(cleanComment.startsWith("-OUTPUT:") && cleanComment.endsWith("-"))
        val expectedOutput = cleanComment.drop("-OUTPUT:\n".length).dropRight(1)
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
        actualOutput should be(expectedOutput)
      }
    }
  }
}
