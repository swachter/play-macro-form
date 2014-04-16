package eu.swdev.i18n

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context
import scala.language.experimental.macros
import java.util.Locale
import scala.io.Source
import java.net.URLClassLoader

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
      val is = this.getClass.getResourceAsStream("/application.conf")
      if (is != null) {
        try {
          Source.fromInputStream(is, "UTF-8").getLines().filter(_.matches(pattern + ".*")).flatMap(_ match {
            case RegEx(langs) => {
              // the value of the langs attribute may be enclosed in quotation marks
              // -> allow quotation marks as separators -> they will be ignored
              langs.trim.split("""\s*,\s*|"|\s+""").filter(!_.trim.isEmpty).map(s => stringToLocale((s)))
            }
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

    def createParams(prefix: String, nb: Int, typeName: String): List[ValDef] = {
      (for {
        i <- 0 until nb
      } yield {
        val paramName = s"$prefix$i"
        // create a method definition with a single parameter and extract that parameter
        q"""def a(${newTermName(paramName)}: ${newTypeName(typeName)})""" match { case q"""def ${_}($x)""" => x }
      }).toList
    }

    def createArgParams(tpe: EntryType): List[ValDef] = {
      createParams("arg", tpe.args, "AnyRef")
    }
    /**
     * Calls the appropriate output method.
     *
     * The output method is determined by the isMarkup property of the specified ResType. If isMarkup = true then the
     * method `outputMarkup` is called and otherwise `outputRaw`.
     *
     * {{{
     *   $x.<output method>(<args>)
     * }}}
     *
     * @param x
     * @param tpe
     * @return
     */
    def callOutputMethod(x: Tree, tpe: EntryType): Tree = {
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
    def mapOutputMethod(x: Tree, tpe: EntryType): Tree = {
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

    def createMethodName(name: String) = c.universe.newTermName(name.replace('.', '_'))

    def simpleMsgDef(name: String, tpe: MsgEntryType): Tree = {
      val methodName = createMethodName(name)
      val lhs = callOutputMethod(q"entriesMap(locale)($name)", tpe)
      // the implicit MsgMarkup parameter is necessary for markup messages only. However, in order to have a stable
      // call-signature (i.e. the signature without considering the result type) the implicit MsgMarkup parameter
      // is always created. This provides stability when the generated methods implement interfaces. (Otherwise the
      // interface would change every time an entry changes its isMarkup property.)
      if (tpe.args == 0) {
        q"""def $methodName(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
      } else {
        val args = createArgParams(tpe)
        q"""def $methodName(..$args)(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
      }
    }

    def lookupMsgDef(name: String, tpe: LookupEntryType): Tree = {
      val methodName = createMethodName(name)

      def calcNbKeys(t: EntryType): Int = t match {
        case TreeEntryType(nested) => 1 + calcNbKeys(nested)
        case MapEntryType(nested) => 1 + calcNbKeys(nested)
        case _: MsgEntryType => 0
      }

      val nbKeys = calcNbKeys(tpe)

      val keyParams = createParams("key", nbKeys, "String")

      val start = q"entriesMap(locale)($name).lookup(key0)"

      val flat = flatMapLookup(start, nbKeys - 1)

      val lhs = mapOutputMethod(flat, tpe)

      if (tpe.args == 0) {
        q"""def $methodName(..$keyParams)(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
      } else {
        val args = createArgParams(tpe)
        q"""def $methodName(..$keyParams)(..$args)(implicit locale: Locale, markup: MsgMarkup) = $lhs"""
      }
    }

    def quoteLocale(l: Locale): Tree = {
      q"new Locale(${l.getLanguage}, ${l.getCountry}, ${l.getVariant})"
    }

    def constructNewBody(body: List[Tree]): List[Tree] = {

      val macroParams = c.macroApplication match {
        case q"new ${_}(..$mps).${_}(${_})" => extractMacroParams(mps)
        case _ => c.abort(c.enclosingPosition, "could not extract macro parameters")
      }

      val resourcePath = macroParams.resourcePath.getOrElse("conf/messages")
      val locales: List[Locale] = macroParams.locales.getOrElse(guessLocales)

      c.info(c.enclosingPosition, s"processing resources - resourcePath: $resourcePath; locales: $locales", true)

//      println(s"c.classPath: ${c.classPath}")
//      val urlClassLoader = new URLClassLoader(c.classPath.toArray)
//      val url = urlClassLoader.findResource("form/form-resource")
//      println(s"url: $url")

      val either = Analyzer.analyze(this.getClass.getClassLoader, resourcePath, locales: _*)
      //println(s"result: $either")

      if (either.isLeft) {
        c.abort(c.enclosingPosition, s"""resources could not be analyzed - ${either.left.get}""")
      }
      val result = either.right.get
      if (result.resultsOfOneLocale.values.exists(!_.unprocessed.isEmpty)) {
        c.abort(c.enclosingPosition, s"""some resource entries could not be resolved - ${result.resultsOfOneLocale.mapValues(_.unprocessed)}""")
      }
      if (result.missing.values.exists(!_.isEmpty)) {
        val info = result.missing.filterKeys(!result.missing(_).isEmpty).map(t => s"  [${t._1} -> ${t._2}]").mkString("\n")
        c.abort(c.enclosingPosition, s"""missing resource entries -\n$info""")
      }
      if (!result.conflicting.isEmpty) {
        c.abort(c.enclosingPosition, s"""some resource entries have conflicting types - ${result.conflicting}""")
      }

      val quotedLocales: List[Tree] = locales.map(quoteLocale(_))

      val simpleMsgDefs = (for {
        x <- result.types.collect{ case (n, t: MsgEntryType) => (n, t) }
      } yield {
        simpleMsgDef(x._1, x._2)
      }).toList

      val lookupMsgDefs = (for {
        x <- result.types.collect{ case (n, t: LookupEntryType) => (n, t) }
      } yield {
        lookupMsgDef(x._1, x._2)
      }).toList


      //println(s"simpleMsgDefs: ${simpleMsgDefs.length}")
      //println(s"lookupMsgDefs: ${lookupMsgDefs.length}")

      val tbody = body.asInstanceOf[List[Tree]]

      val newBody = q"""
              import eu.swdev.i18n.MsgMarkup
              import java.util.Locale
              val entriesMap = eu.swdev.i18n.ResourcesLoader.loadEntries(getClass.getClassLoader, $resourcePath, List(..$quotedLocales))
              ..$simpleMsgDefs
              ..$lookupMsgDefs
              ..$tbody
      """

      newBody match {
        case Block(l, _) => l
        case _ => throw new Exception(s"unexpected new body: ${c.universe.showRaw(newBody)}")
      }
    }

    //
    // macro execution starts here
    //

    //c.info(c.enclosingPosition, "executing macro", true)

    val modDefs: List[Tree] = annottees.map {
      annottee => {
        annottee.tree match {
          case q"object $name extends ..$baseTypes { ..$body }" => {
            val newBody = constructNewBody(body)
            q"object $name extends ..$baseTypes { ..$newBody }"
          }
          case q"class $name extends ..$baseTypes { ..$body }" => {
            val newBody = constructNewBody(body)
            q"class $name extends ..$baseTypes { ..$newBody }"
          }
          case q"class $name(..$args) extends ..$baseTypes { ..$body }" => {
            val newBody = constructNewBody(body)
            q"class $name(..$args) extends ..$baseTypes { ..$newBody }"
          }
          case q"trait $name extends ..$baseTypes { ..$body }" => {
            val newBody = constructNewBody(body)
            q"trait $name extends ..$baseTypes { ..$newBody }"
          }
          case q"trait $name extends ..$baseTypes" => {
            val newBody = constructNewBody(Nil)
            q"trait $name extends ..$baseTypes { ..$newBody }"
          }
          case x => x
        }
      }
    }.toList
    //println("Block: " + Block(modDefs, Literal(Constant())))

    c.Expr(Block(modDefs, Literal(Constant())))

  }

}
