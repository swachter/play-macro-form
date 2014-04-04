package eu.swdev.i18n

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context
import scala.language.experimental.macros
import java.util.Locale
import eu.swdev.i18n.Analyzer.MsgSignature

/**
  */
class Resource(val resourcePath: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ResourceMacro.impl
}

object ResourceMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    def simpleMsgDef(id: String, signature: MsgSignature): Tree = {
      val idName = c.universe.newTermName(id)
      q"""def $idName(implicit locale: Locale) = simpleMsgs(locale)($id).rawMsg(null)"""
    }

    val modDefs: List[Tree] = annottees.map {
      annottee => {
        annottee.tree match {
          case q"object $objectName { ..$body }" => {

            val resourcePath: String = (c.macroApplication match {
              case q"new ${_}(resourcePath=$x).${_}(${_})" => x match {
                case Literal(Constant(s)) => s
                case _ => c.abort(c.enclosingPosition, "could not extract value from resourcePath")
              }
              case _ => c.abort(c.enclosingPosition, "could not detect resourcePath")
            }).asInstanceOf[String]

            println(s"resourcePath: $resourcePath")
            val url = this.getClass.getClassLoader.getResource(resourcePath)
            println(s"url: $url")

            val result = Analyzer.analyze(this.getClass.getClassLoader, resourcePath, new Locale("de", "DE"))

            val simpleMsgDefs = (for {
              id <- result.simpleKeys
            } yield {
              simpleMsgDef(id, result.signatures(id))
            }).toList




//            println("showRaw1: " + c.universe.showRaw(c.macroApplication))
//            val t: Tree = q"""$x.macro@CompiledMessages(resourcePath="abc") object X {}"""
//            println(t)
//            println("showRaw2: " + c.universe.showRaw(t))

            val tbody = body.asInstanceOf[List[Tree]]

            // output the modified object
            q"""
            object $objectName {
              val (simpleMsgs, lookupMsgs) = eu.swdev.i18n.ResourcesLoader.buildMaps(getClass.getClassLoader, $resourcePath, new java.util.Locale("de", "DE"))
              ..$simpleMsgDefs
              ..$tbody
            }"""
          }
          case x => x
        }
      }
    }.toList
    //println("Block: " + Block(modDefs, Literal(Constant())))

    c.Expr(Block(modDefs, Literal(Constant())))

  }

}
