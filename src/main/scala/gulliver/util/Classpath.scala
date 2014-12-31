package gulliver.util

import java.io.File
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.MethodVisitor
import java.util.jar.JarFile
import java.io.InputStream
import org.objectweb.asm.ClassReader
import java.io.FileInputStream
import org.objectweb.asm.FieldVisitor

object Classpath {
  lazy val JavaRuntimeEntries = Entry.fromClasspath(System.getProperty("sun.boot.class.path"))
  lazy val Default = new Classpath(JavaRuntimeEntries)

  object Entry {
    def fromClasspath(classpath: String): Seq[Entry] = {
      classpath.split(File.pathSeparatorChar).toSeq.flatMap { path =>
        val file = new File(path)
        if (file.exists) Some(Entry(file)) else None
      }
    }
    def apply(file: File): Entry = {
      if (file.isDirectory()) EntryDir(file)
      else EntryJar(file)
    }
  }
  
  sealed trait Entry {
    def loadType(fqcn: String): Option[TypeDef]
    def close(): Unit = { }
    
    def typeFromStream(is: InputStream): TypeDef = {
      val visitor = new TypeInfoVisitor
      new ClassReader(is).accept(visitor, 0)
      TypeDef(visitor.typ.get, visitor.methods, visitor.fields)
    }
  }
  
  case class EntryJar(file: File) extends Entry {
    val jarFile = new JarFile(file)
    
    override def loadType(fqcn: String): Option[TypeDef] = {
      val path = fqcn.replace('.', '/') + ".class"
      Option(jarFile.getJarEntry(path)).map { entry =>
        val stream = jarFile.getInputStream(entry)
        try typeFromStream(stream)
        finally stream.close()
      }
    }
    
    override def close(): Unit = jarFile.close()
  }

  case class EntryDir(dir: File) extends Entry {
    override def loadType(fqcn: String): Option[TypeDef] = {
      val file = new File(dir, fqcn.replace('.', '/') + ".class")
      if (!file.exists()) None
      else {
        val stream = new FileInputStream(file)
        try Some(typeFromStream(stream))
        finally stream.close()
      }
    }
  }
  
  // TODO: type params
  case class TypeDef(info: TypeInfo, methods: Map[String, Seq[MethodInfo]], fields: Map[String, FieldInfo])
  case class TypeInfo(mods: Int, typ: TypeForm.Value, name: String, ext: Option[String], interfaces: Seq[String])
  object TypeForm extends Enumeration {
    val Class, Interface, Enum, Annotation = Value
  }
  
  // TODO: type params
  case class MethodInfo(mods: Int, name: String, ret: Type, params: Seq[Type])
  case class FieldInfo(mods: Int, name: String, typ: Type)
  
  class TypeInfoVisitor extends ClassVisitor(Opcodes.ASM5) {
    var fields = Map.empty[String, FieldInfo]
    var methods = Map.empty[String, Seq[MethodInfo]]
    var typ = Option.empty[TypeInfo]
    
    override def visit(version: Int, access: Int, name: String, signature: String,
        superName: String, interfaces: Array[String]): Unit = {
      val form =
        if ((access & Opcodes.ACC_ANNOTATION) != 0) TypeForm.Annotation
        else if ((access & Opcodes.ACC_ENUM) != 0) TypeForm.Enum
        else if ((access & Opcodes.ACC_INTERFACE) != 0) TypeForm.Interface
        else TypeForm.Class
      typ = Some(TypeInfo(access, form, name, Option(superName),
        Option(interfaces).map(_.toSeq).getOrElse(Seq.empty)))
    }
    
    override def visitField(access: Int, name: String, desc: String,
        signature: String, value: AnyRef): FieldVisitor = {
      fields += name -> FieldInfo(access, name, Type.getType(desc))
      null
    }
    
    override def visitMethod(access: Int, name: String, desc: String,
        signature: String, exceptions: Array[String]): MethodVisitor = {
      val info = MethodInfo(access, name, Type.getReturnType(desc), Type.getArgumentTypes(desc).toSeq)
      methods.get(name) match {
        case None => methods += name -> Seq(info)
        case Some(seq) => methods += name -> (seq :+ info)
      }
      null
    }
  }
}

class Classpath(entries: Seq[Classpath.Entry]) {
  import Classpath._
  
  var typeCache = Map.empty[String, Option[TypeDef]]
  
  def loadType(fqcn: String): Option[TypeDef] = typeCache.getOrElse(fqcn, {
    val typ = entries.toStream.flatMap(_.loadType(fqcn)).headOption
    typeCache += fqcn -> typ
    typ
  })
  
  def close(): Unit = entries.foreach(_.close())
}