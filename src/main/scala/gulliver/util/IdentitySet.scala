package gulliver.util

import java.util.IdentityHashMap
import java.util.Collections

import gulliver.parse.Ast

import scala.collection.JavaConversions._

object IdentitySet {
 def apply[T](): Set[T] = Collections.newSetFromMap(new IdentityHashMap[T, java.lang.Boolean]).toSet
}
