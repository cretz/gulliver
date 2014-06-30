package gulliver.transform

import gulliver.util.Classpath

class Context(val classpath: Classpath) {
  // Keyed by package name
  var modules = Map.empty[String, Module]

  def getOrCreateModule(name: String): Module = modules.getOrElse(name, {
    val mod = Module(this, name)
    modules += name -> mod
    mod
  })
}
