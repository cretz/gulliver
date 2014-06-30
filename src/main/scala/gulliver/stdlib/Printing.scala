package gulliver.stdlib

import gulliver.transform.Embedded

object Printing extends Embedded {

  /* Something like this...
  val embeds = Map(
    FuncEmbed(None, "print") -> print,
    FuncEmbed(None, "println") -> println
  )

  def print(f: Func, params: Ref.VarRef*): Unit = doSystemCall(f, "println", params:_*)

  def println(f: Func, params: Ref.VarRef*): Unit = doSystemCall(f, "println", params:_*)

  def doSystemCall(f: Func, name: String, params: Ref.VarRef*): Unit = {
    require(params.size <= 1, s"$name accepts no more than one parameter")
    val out = f.mod.getStaticField("java.lang.System", "out")
    if (params.isEmpty) {
      val func = out.getMethod(name, Seq(params.head), TypeId.Void)
      func.invoke(params.head)
    } else {
      val func = out.getMethod(name, Seq.empty, TypeId.Void)
      func.invoke()
    }
  }
  */
}
