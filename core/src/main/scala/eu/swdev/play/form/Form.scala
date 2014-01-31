package eu.swdev.play.form

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

/**
  */
class Form extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro identityMacro.impl
}

object identityMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val (annottee, expandees) = inputs match {
      case (param: ValDef) :: (rest@(_ :: _)) => (param, rest)
      case (param: TypeDef) :: (rest@(_ :: _)) => (param, rest)
      case _ => (EmptyTree, inputs)
    }
    println((annottee, expandees))
    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}