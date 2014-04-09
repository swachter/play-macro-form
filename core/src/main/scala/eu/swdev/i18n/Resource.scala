package eu.swdev.i18n

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context
import scala.language.experimental.macros
import java.util.Locale
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

    /**
     * Creates a tree for the argument array that is passed into the output methods.
     *
     * {{{
     *   Array()
     *   Array(arg0)
     *   Array(arg1, arg2)
     * }}}
     *
     * @param args
     * @return
     */
    def createArgsArray(args: Int): Tree = {
      val argList = (for {
        i <- 0 until args
      } yield {
        Ident(newTermName(s"arg$i"))
      }).toList
      Apply(Ident(newTermName("Array")), argList)
    }

    /**
     * Creates a method parameter definition.
     *
     * @param name
     * @return
     */
    def valDef(name: String): ValDef = {
      q"""def a(${newTermName(name)}: String): String""" match { case q"""def ${_}($x): String""" => x }
    }

    def createParams(prefix: String, nb: Int): List[ValDef] = {
      (for {
        i <- 0 until nb
      } yield {
        valDef(s"$prefix$i")
      }).toList
    }

    /**
     * Calls the appropriate output method.
     *
     * The output method is determined by the isMarkup property of the specified ResType.
     *
     * {{{
     *   $x.<output method>(<args>)
     * }}}
     *
     * @param x
     * @param tpe
     * @return
     */
    def callOutputMethod(x: Tree, tpe: ResType): Tree = {
      val methodName = c.universe.newTermName(if (tpe.isMarkup) "outputMarkup" else "outputRaw")
      val argsArray = createArgsArray(tpe.args)
      q"$x.$methodName($argsArray)"
    }

    /**
     * Maps the appropriate output method.
     *
     * {{{
     *   $x.map(_.<output method>(<args>))
     * }}}
     *
     * @param x
     * @param tpe
     * @return
     */
    def mapOutputMethod(x: Tree, tpe: ResType): Tree = {
      val methodName = c.universe.newTermName(if (tpe.isMarkup) "outputMarkup" else "outputRaw")
      val argsArray = createArgsArray(tpe.args)
      q"$x.map(_.$methodName($argsArray))"
    }

    /**
     * Recursively applies flatMap(_.lookup(key)) calls.
     *
     * {{{
     *   $x
     *   $x.flatMap(_.lookup(key1))
     *   $x.flatMap(_.lookup(key1)).flatMap(_.lookup(key2))
     * }}}
     * @param x
     * @param key
     * @return
     */
    def flatMapLookup(x: Tree, key: Int): Tree = {
      if (key == 0) {
        x
      } else {
        val tmp = flatMapLookup(x, key - 1)
        q"$tmp.flatMap(_.lookup(${newTermName(s"key$key")}))"
      }
    }

    def simpleMsgDef(id: String, tpe: MsgResType): Tree = {
      val idName = c.universe.newTermName(id)
      val lhs = callOutputMethod(q"resMap(locale)($id)", tpe)
      if (tpe.args == 0) {
        q"""def $idName(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
      } else {
        val args = createParams("arg", tpe.args)
        q"""def $idName(..$args)(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
      }
    }

    def lookupMsgDef(id: String, tpe: TreeResType): Tree = {
      val idName = c.universe.newTermName(id)
      val methodName = c.universe.newTermName(if (tpe.isMarkup) "markupMsg" else "rawMsg")

      def calcNbKeys(t: ResType): Int = t match {
        case TreeResType(nested) => 1 + calcNbKeys(nested)
        case _: MsgResType => 0
      }

      val nbKeys = calcNbKeys(tpe)

      val keyParams = createParams("key", nbKeys)

      val start = q"resMap(locale)($id).lookup(key0)"

      val flat = flatMapLookup(start, nbKeys - 1)

      val lhs = mapOutputMethod(flat, tpe)

      if (tpe.args == 0) {
        q"""def $idName(..$keyParams)(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
      } else {
        val args = createParams("arg", tpe.args)
        q"""def $idName(..$keyParams)(..$args)(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
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

            if (result.unresolved.values.exists(!_.isEmpty)) {
              c.abort(c.enclosingPosition, s"""some resource entries could not be resolved - ${result.unresolved}""")
            }
            if (result.missing.values.exists(!_.isEmpty)) {
              val info = result.missing.filterKeys(!result.missing(_).isEmpty).map(t => s"  [${t._1} -> ${t._2}]").mkString("\n")
              c.abort(c.enclosingPosition, s"""missing resource entries -\n$info""")
            }
            if (!result.conflicting.isEmpty) {
              c.abort(c.enclosingPosition, s"""some resource entries have conflicting types - ${result.conflicting}""")
            }

            println(s"result: ${result}")
            println(s"result.types: ${result.types}")

            val simpleMsgDefs = (for {
              x <- result.types.collect{ case (n, t@MsgResType(_, _)) => (n, t) }
            } yield {
              simpleMsgDef(x._1, x._2)
            }).toList

            val lookupMsgDefs = (for {
              x <- result.types.collect{ case (n, t@TreeResType(_)) => (n, t) }
            } yield {
              lookupMsgDef(x._1, x._2)
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
              val resMap = eu.swdev.i18n.ResourcesLoader.load(getClass.getClassLoader, $resourcePath, new Locale("de", "DE"))
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
