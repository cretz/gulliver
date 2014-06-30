package gulliver.transform

sealed trait TypeRef {
  def desc(): String
}

object TypeRef {
  def apply(str: String): TypeRef = str match {
    // TODO: primitives
    case s => TypeRef.Object(str)
  }

  val String = apply("java.lang.String")

  case class Array(of: TypeRef) extends TypeRef {
    lazy val desc = '[' + of.desc
  }
  case object Boolean extends TypeRef {
    val desc = "Z"
  }
  case object Byte extends TypeRef {
    val desc = "B"
  }
  case object Char extends TypeRef {
    val desc = "C"
  }
  case object Failure extends TypeRef {
    def desc(): String = ???
  }
  case object Float extends TypeRef {
    val desc = "F"
  }
  case object Int extends TypeRef {
    val desc = "I"
  }
  case object Long extends TypeRef {
    val desc = "J"
  }
  case object Nothing extends TypeRef {
    def desc(): String = ???
  }
  case class Object(fqcn: String) extends TypeRef {
    lazy val desc = 'L' + fqcn.replace('.', '/') + ';'
  }
  case object Short extends TypeRef {
    val desc = "S"
  }
  case object Void extends TypeRef {
    val desc = "V"
  }
}
