package gulliver.compile

import scala.collection.JavaConversions._
import gulliver.util.Classpath
import gulliver.parse.Ast
import monocle._
import monocle.macros._

object JavaModel {

  sealed trait Context {
    var namedRefs = Map.empty[String, Seq[Ref]]
    
    def addNamedRef(name: String, ref: Ref) = {
      namedRefs += name -> (ref +: namedRefs.getOrElse(name, Seq.empty))
    }
    
    def addStatement(stmt: JAst.Stmt): Unit = parent.addStatement(stmt)
    
    def classpath: Classpath = parent.classpath
    
    def findRefs(name: String): Seq[Ref] = 
      namedRefs.getOrElse(name, Seq.empty) ++
        parentOption.map(_.findRefs(name)).getOrElse(Seq.empty)
    
    def findTypeRef(fqcn: String): Option[Ref.TypeRef] = {
      classpath.getType(fqcn).map(Ref.ClasspathTypeRef)
    }
    
    def getPkg(pkgName: String): Option[Pkg] = parent.getPkg(pkgName)
    def getOrCreatePkg(pkgName: String): Pkg = parent.getOrCreatePkg(pkgName)

    def importPkg(pkgName: String): Boolean = {
      getPkg(pkgName).foreach { _ =>
        ???
      }
      val defs = classpath.getPkgTypes(pkgName)
      defs.foreach { typeDef =>
        addNamedRef(typeDef.info.className, Ref.ClasspathTypeRef(typeDef))
      }
      !defs.isEmpty
    }
    
    def parentOption = Option.empty[Context]
    
    def parent = parentOption.getOrElse(sys.error("No parent"))
  }
  
  class GlobalContext(override val classpath: Classpath) extends Context {
    // Keyed by relative path
    var units = Map.empty[String, JAst.CompilationUnit]
    
    // Keyed by package name
    var packages = Map.empty[String, Pkg]
    
    def getAllUnits(): Map[String, JAst.CompilationUnit] = {
      units ++ packages.map {
        case (k, v) => k.replace('.', '/') + "/module" -> v.unit
      }
    }
    
    override def getPkg(pkgName: String) = packages.get(pkgName)
    
    override def getOrCreatePkg(pkgName: String): Pkg = packages.getOrElse(pkgName, {
      val pkg = new Pkg(pkgName)
      packages += pkgName -> pkg
      pkg
    })
  }
  
  class FileContext(global: GlobalContext, pkg: Pkg) extends Context {
    override val parentOption = Some(global)
    
    override def addStatement(stmt: JAst.Stmt): Unit = {
      pkg.addStmt(stmt)
    }
  }
  
//  object Lenses {
//    val temp1 = Lenser[JAst.CompilationUnit]
//    val temp: Lens[JAst.CompilationUnit, Seq[JAst.BaseTypeDecl]] = temp1(_.types)
//    val temp2 = temp1(_.types)
//  }
  
  class Pkg(val name: String) {
    var unit = JAst.CompilationUnit(
      pkg = Some(JAst.PackageDecl(name = name.toName)),
      imports = Seq.empty,
      types = Seq(
        JAst.TypeDecl(
          name = JAst.SimpleName("module"),
          mods = Seq(JAst.PublicMod, JAst.FinalMod),
          bodyDecls = Seq(
            // Static init (to be updated w/ stmts later)
            JAst.Initializer(
              mods = Seq(JAst.StaticMod)
            ),
            // Empty main...
            JAst.MethodDecl(
              mods = Seq(JAst.PublicMod, JAst.StaticMod),
              name = JAst.SimpleName("main"),
              params = Seq(
                JAst.SingleVarDecl(
                  name = JAst.SimpleName("args"),
                  typ = JAst.ArrayType(
                    typ = JAst.SimpleType("java.lang.String".toName),
                    dims = Seq(JAst.Dimension())
                  )
                )
              ),
              retType = Some(JAst.PrimitiveType(JAst.VoidPrimitive)),
              body = Some(JAst.Block())
            )
          )
        )
      )
    )
    
    // TODO: lenses?
    
    def addStmt(stmt: JAst.Stmt): Unit = {
      val init = unit.types.head.bodyDecls.head.asInstanceOf[JAst.Initializer]
      unit = unit.copy(
        types = unit.types.updated(0,
          unit.types.head.asInstanceOf[JAst.TypeDecl].copy(
            bodyDecls = unit.types.head.bodyDecls.updated(0,
              init.copy(
                body = init.body.copy(
                  stmts = init.body.stmts :+ stmt
                )
              )
            )
          )
        )
      )
    }
  }
  
//    import scala.language.implicitConversions
  
  implicit class PimpedString(str: String) {
    def toName: JAst.Name = {
      val pieces = str.split('.')
      pieces.tail.foldLeft(pieces.head.toSimpleName: JAst.Name) {
        case (name, piece) => JAst.QualifiedName(name, piece.toSimpleName)
      }
    }
    
    def toSimpleName: JAst.SimpleName = JAst.SimpleName(str)
    
    def toType: JAst.Type = JAst.SimpleType(name = toName)
  }
  
  implicit class PimpedTypeRef(ref: Ref.TypeRef) {
    // TODO: handle more options
    def toType: JAst.Type = (ref.pkgName + '.' + ref.className).toType
    
    def findChildRefs(ctx: Context, name: String): Seq[Ref] = {
      ref.field(name).toSeq ++ ref.method(name)
    }
  }
  
  implicit class PimpedMethodRef(ref: JAst.MethodRef) {
    def findAppropriateParamSet(paramSet: Seq[Seq[JAst.Expr]]): Option[Seq[JAst.Expr]] = {
      // TODO: proper type checking
      paramSet.headOption
    }
  }
  
  implicit class PimpedType(typ: JAst.Type) {
    def fqcn: String = typ match {
      case t: JAst.SimpleType => t.name.fullString
      case _ => ???
    }
    
    def toTypeRef(ctx: Context): Option[Ref.TypeRef] = ctx.findTypeRef(fqcn)
    
    
  }
  
  implicit class PimpedName(name: JAst.Name) {
    def fullString: String = name match {
      case JAst.SimpleName(n) => n
      case JAst.QualifiedName(qual, JAst.SimpleName(n)) => qual.fullString + '.' + n
    }
  }
  
  implicit class PimpedNode[T <: JAst.Node](node: T) {
    def setSourceAst(sourceAst: AnyRef): T = {
      node.compileTimeParams += "gulliver.sourceAst" -> sourceAst
      node
    }
    
    def getSourceAst[U <: AnyRef](): Option[U] =
      node.compileTimeParams.get("gulliver.sourceAst").map(_.asInstanceOf[U])
      
    def setStaticType(typ: JAst.Type): T = {
      node.compileTimeParams += "gulliver.staticType" -> typ
      node
    }
      
    def getStaticType(): Option[JAst.Type] =
      node.compileTimeParams.get("gulliver.staticType").map(_.asInstanceOf[JAst.Type])
      
    def setType(typ: JAst.Type): T = {
      node.compileTimeParams += "gulliver.type" -> typ
      node
    }
      
    def getType(): Option[JAst.Type] =
      node.compileTimeParams.get("gulliver.type").map(_.asInstanceOf[JAst.Type])
      
    def setRef(ref: Ref): T = {
      node.compileTimeParams += "gulliver.ref" -> ref
      node
    }
    
    def getRef(): Option[Ref] =
      node.compileTimeParams.get("gulliver.ref").map(_.asInstanceOf[Ref])
    
    def getEitherType(): Option[JAst.Type] = {
      node.getType().orElse(node.getStaticType())
    }
    
    def findChildRefs(ctx: Context, name: String): Seq[Ref] = {
      getEitherType().flatMap(_.toTypeRef(ctx)).
        map(_.findChildRefs(ctx, name)).getOrElse(Seq.empty)
    }
  }
  
}