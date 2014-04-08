package eu.swdev.i18n

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context
import scala.language.experimental.macros
import java.util.Locale
import eu.swdev.i18n.Analyzer.MsgSignature
import scala.io.Source

/**
 *
 */
class Resource(val resourcePath: String = null, val locales: List[String] = null) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ResourceMacro.impl
}

object ResourceMacro {

  case class MacroParams(resourcePath: Option[String], locales: Option[List[Locale]])

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    def normalizeAssign(tree: Tree): Tree = tree match {
      case AssignOrNamedArg(l, r) => Assign(l, r)
      case _ => tree
    }

    def stringToLocale(s: String): Locale = {
      val split = s.split("_")
      split.length match {
        case 1 => new Locale(split(0))
        case 2 => new Locale(split(0), split(1))
        case 3 => new Locale(split(0), split(1), split(2))
        case _ => c.abort(c.enclosingPosition, s"could not extract locale - param: $s")
      }
    }

    def extractLocale(tree: Tree): Locale = {
      tree match {
        case Literal(Constant(string)) => {
          val s = string.asInstanceOf[String]
          stringToLocale(s)
        }
        case _ => c.abort(c.enclosingPosition, s"could not extract locale - param: $tree; raw: ${c.universe.showRaw(tree)}")
      }
    }
    def extractMacroParams(list: List[Tree]): MacroParams = list match {
      case Nil => MacroParams(None, None)
      case tree :: tail => {
        val mp = extractMacroParams(tail)
        normalizeAssign(tree) match {
          case q"resourcePath=$x" => x match {
            case Literal(Constant(string)) => mp.copy(resourcePath = Some(string.asInstanceOf[String]))
            case _ => c.abort(c.enclosingPosition, s"could not extract value of resourcePath - lhs: $x")
          }
          case q"locales=List(..$l)" => {
            mp.copy(locales = Some(l.map(t => extractLocale(t))))
          }
          case _ => c.abort(c.enclosingPosition, s"could not interpret macro parameter - param: $tree; raw: ${c.universe.showRaw(tree)}")
        }
      }
    }

    def guessLocales: List[Locale] = {
      // check if the macro is used inside a Play application, i.e. if the resource /conf/application.conf exists
      // in that case the applications.langs entry is evaluated
      val pattern = """^\s*application.langs\s*(?:=|:)"""
      val RegEx = (pattern + "(.*)").r
      val is = this.getClass.getResourceAsStream("/conf/application.conf")
      if (is != null) {
        try {
          Source.fromInputStream(is, "UTF-8").getLines().filter(_.matches(pattern + ".*")).flatMap(_ match {
            case RegEx(langs) => langs.trim.split(""",|\s+""").filter(!_.trim.isEmpty).map(s => stringToLocale((s)))
          }).toList
        } finally {
          is.close()
        }
      } else {
        Nil
      }
    }

    def simpleMsgDef(id: String, signature: MsgSignature): Tree = {
      val idName = c.universe.newTermName(id)
      val methodName = c.universe.newTermName(if (signature.isMarkup) "markupMsg" else "rawMsg")
      signature.args match {
        case 0 => q"""def $idName(implicit locale: Locale, markup: MsgMarkup) = simpleMsgs(locale)($id).$methodName(null)"""
        case 1 => q"""def $idName(arg0: AnyRef)(implicit locale: Locale, markup: MsgMarkup) = simpleMsgs(locale)($id).$methodName(Array(arg0))"""
        case 2 => q"""def $idName(arg0: AnyRef, arg1: AnyRef)(implicit locale: Locale, markup: MsgMarkup) = simpleMsgs(locale)($id).$methodName(Array(arg0, arg1))"""
        case 3 => q"""def $idName(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale, markup: MsgMarkup) = simpleMsgs(locale)($id).$methodName(Array(arg0, arg1, arg2))"""
      }
    }

    def lookupMsgDef(id: String, signature: MsgSignature): Tree = {
      val idName = c.universe.newTermName(id)
      val methodName = c.universe.newTermName(if (signature.isMarkup) "markupMsg" else "rawMsg")
      signature.args match {
        case 0 => q"""def $idName(path: String)(implicit locale: Locale, markup: MsgMarkup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(null))"""
        case 1 => q"""def $idName(path: String)(arg0: AnyRef)(implicit locale: Locale, markup: MsgMarkup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(Array(arg0)))"""
        case 2 => q"""def $idName(path: String)(arg0: AnyRef, arg1: AnyRef)(implicit locale: Locale, markup: MsgMarkup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(Array(arg0, arg1)))"""
        case 3 => q"""def $idName(path: String)(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale, markup: MsgMarkup) = lookupMsgs(locale)($id).getValue(path).map(_.$methodName(Array(arg0, arg1, arg2)))"""
      }
    }

    val modDefs: List[Tree] = annottees.map {
      annottee => {
        annottee.tree match {
          case q"object $objectName { ..$body }" => {

            val macroParams = c.macroApplication match {
              case q"new ${_}(..$mps).${_}(${_})" => extractMacroParams(mps)
              case _ => c.abort(c.enclosingPosition, "could not extract macro parameters")
            }

            val resourcePath = macroParams.resourcePath.getOrElse("conf/messages")
            val locales = macroParams.locales.getOrElse(guessLocales)

            c.info(c.enclosingPosition, s"processing resources - resourcePath: $resourcePath; locales: $locales", true)

            val result = Analyzer.analyze(this.getClass.getClassLoader, resourcePath, locales: _*)

            if (result.missingKeys.values.exists(!_.isEmpty)) {
              val info = result.missingKeys.filterKeys(!result.missingKeys(_).isEmpty).map(t => s"  [${t._1} -> ${t._2}]").mkString("\n")
              c.abort(c.enclosingPosition, s"""missing resource entries -\n$info""")
            }
            if (!result.ambiguouslyUsedKeys.isEmpty) {
              c.abort(c.enclosingPosition, s"""some resource entries that are defined as simple resource and as lookup resources - ${result.ambiguouslyUsedKeys}""")
            }

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
              import eu.swdev.i18n.MsgMarkup
              import java.util.Locale
              val (simpleMsgs, lookupMsgs) = eu.swdev.i18n.ResourcesLoader.buildMaps(getClass.getClassLoader, $resourcePath, new Locale("de", "DE"))
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
