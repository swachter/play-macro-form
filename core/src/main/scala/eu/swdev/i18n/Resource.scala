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
      val methodName = c.universe.newTermName(if (signature.isMarkup) "markupMsg" else "rawMsg")
      signature.args match {
        case 0 => q"""def $idName(implicit locale: Locale, markup: Markup) = simpleMsgs(locale)($id).$methodName(null)"""
        case 1 => q"""def $idName(arg0: AnyRef)(implicit locale: Locale, markup: Markup) = simpleMsgs(locale)($id).$methodName(Array(arg0))"""
        case 2 => q"""def $idName(arg0: AnyRef, arg1: AnyRef)(implicit locale: Locale, markup: Markup) = simpleMsgs(locale)($id).$methodName(Array(arg0, arg1))"""
        case 3 => q"""def $idName(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale, markup: Markup) = simpleMsgs(locale)($id).$methodName(Array(arg0, arg1, arg2))"""
      }
    }

    def lookupMsgDef(id: String, signature: MsgSignature): Tree = {
      val idName = c.universe.newTermName(id)
      val methodName = c.universe.newTermName(if (signature.isMarkup) "markupMsg" else "rawMsg")
      signature.args match {
        case 0 => q"""def $idName(path: String)(implicit locale: Locale, markup: Markup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(null))"""
        case 1 => q"""def $idName(path: String)(arg0: AnyRef)(implicit locale: Locale, markup: Markup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(Array(arg0)))"""
        case 2 => q"""def $idName(path: String)(arg0: AnyRef, arg1: AnyRef)(implicit locale: Locale, markup: Markup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(Array(arg0, arg1)))"""
        case 3 => q"""def $idName(path: String)(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale, markup: Markup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(Array(arg0, arg1, arg2)))"""
      }
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

            val lookupMsgDefs = (for {
              id <- result.lookupKeys
            } yield {
              lookupMsgDef(id, result.signatures(id))
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
              ..$lookupMsgDefs
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
