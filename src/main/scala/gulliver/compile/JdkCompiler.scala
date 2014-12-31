package gulliver.compile

import gulliver.util.Classpath
import javax.tools.ToolProvider
import javax.tools.SimpleJavaFileObject
import java.net.URI
import javax.tools.JavaFileObject
import javax.tools.JavaFileManager
import javax.tools.ForwardingJavaFileManager
import scala.collection.JavaConversions._
import javax.tools.JavaFileManager.Location
import javax.tools.FileObject
import java.io.ByteArrayOutputStream
import java.io.OutputStream

class JdkCompiler(classpath: Classpath, code: Map[String, String]) {
  
  var out = Map.empty[String, BytesOutputJavaFileObject]

  def compile(): Compiler.Result = {
    val compiler = ToolProvider.getSystemJavaCompiler()
    val fm = new InternalFileManager(compiler.
      getStandardFileManager(null, null, null))
    val cu = code.map { case (k, v) => new StringInputJavaFileObject(k + ".java", v) }
    val task = compiler.getTask(null, fm, null, null, null, cu.toSeq)
    task.call()
    //Map[String, Array[Byte]]
    val results = out.mapValues { obj =>
      val bytes = obj.stream.toByteArray()
      obj.stream.close()
      bytes
    }
    Left(results)
  }
  
  class InternalFileManager(f: JavaFileManager)
      extends ForwardingJavaFileManager[JavaFileManager](f) {

    override def getFileForOutput(location: Location, packageName: String,
        relativeName: String, sibling: FileObject): FileObject = ???

    override def getJavaFileForOutput(location: Location, className: String,
        kind: JavaFileObject.Kind, sibling: FileObject): JavaFileObject = {
      require(kind == JavaFileObject.Kind.CLASS)
      val fileName = className.replace('.', '/') + ".class"
      val obj = new BytesOutputJavaFileObject(fileName)
      out += fileName -> obj
      obj
    }
  }
  
  class StringInputJavaFileObject(fileName: String, string: String)
      extends SimpleJavaFileObject(new URI(fileName), JavaFileObject.Kind.SOURCE) {
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = string
  }
  
  class BytesOutputJavaFileObject(fileName: String)
      extends SimpleJavaFileObject(new URI(fileName), JavaFileObject.Kind.CLASS) {
    val stream = new ByteArrayOutputStream()
    override def openOutputStream(): OutputStream = stream
  }
}