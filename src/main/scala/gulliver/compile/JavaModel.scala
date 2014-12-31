package gulliver.compile

import org.eclipse.jdt.core.dom
import scala.collection.JavaConversions._

class JavaModel(ast: dom.AST = dom.AST.newAST(dom.AST.JLS8)) {
  
  sealed trait Context {
    def module: dom.CompilationUnit = parent.module
    
    def parentOption = Option.empty[Context]
    
    def parent = parentOption.getOrElse(sys.error("No parent"))
    
    def pkgName: String = parent.pkgName
  }
  
  class GlobalContext extends Context {
    
  }
  
  class PackageContext(global: GlobalContext, override val pkgName: String,
      override val module: dom.CompilationUnit) extends Context {
    override val parentOption = Some(global)
    
    lazy val moduleType: dom.TypeDeclaration = {
      val decl = ast.newTypeDeclaration()
      decl.setName(ast.newSimpleName("module"))
      decl.modifiers().addRef(ast.newModifier(dom.Modifier.ModifierKeyword.FINAL_KEYWORD))
      decl.modifiers().addRef(ast.newModifier(dom.Modifier.ModifierKeyword.PUBLIC_KEYWORD))
      module.types().addRef(decl)
      // Create main
      val func = ast.newMethodDeclaration()
      func.setName(ast.newSimpleName("main"))
      func.modifiers().addRef(ast.newModifier(dom.Modifier.ModifierKeyword.STATIC_KEYWORD))
      func.modifiers().addRef(ast.newModifier(dom.Modifier.ModifierKeyword.PUBLIC_KEYWORD))
      val argsArray = ast.newSingleVariableDeclaration()
      argsArray.setType(ast.newArrayType(ast.newSimpleType(ast.newName("java.lang.String"))))
      argsArray.setName(ast.newSimpleName("args"))
      func.parameters().addRef(argsArray)
      func.setBody(ast.newBlock())
      decl.bodyDeclarations().addRef(func)
      decl
    }
    
    lazy val moduleStaticInit: dom.Initializer = {
      val init = ast.newInitializer()
      init.modifiers().addRef(ast.newModifier(dom.Modifier.ModifierKeyword.STATIC_KEYWORD))
      moduleType.bodyDeclarations().addRef(init)
      init
    }
    
    def addStatement(stmt: dom.Statement): Unit = {
      moduleStaticInit.getBody.statements.addRef(stmt)
    }
  }
  
  sealed trait DomExpr
  
  case class DomExprSimple(expr: dom.Expression) extends DomExpr
  case class DomExprRef(expr: dom.Expression, ref: dom.SimpleName) extends DomExpr
  case class DomExprComposite(exprs: Seq[dom.Expression]) extends DomExpr
  
  // Implicits...
  
  import scala.language.implicitConversions
  
  implicit class JavaListAnyRef(list: java.util.List[_]) {
    @inline
    def toAnyRef = list.asInstanceOf[java.util.List[AnyRef]]
    
    def addRef(v: AnyRef) = toAnyRef.add(v)
    def addAllRefs(v: Seq[AnyRef]) = toAnyRef.addAll(v)
  }
  
  implicit class PimpedNode(node: dom.ASTNode) {
    def setSourceAst(ast: AnyRef): Unit = node.setProperty("gulliver.astNode", ast)
  }
}