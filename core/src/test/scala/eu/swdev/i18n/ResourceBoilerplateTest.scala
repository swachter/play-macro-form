package eu.swdev.i18n

import java.util.Locale
import org.scalatest.FunSuite

/**
  */
object R {

  val (simpleMsgs, lookupMsgs) = ResourcesLoader.buildMaps(getClass.getClassLoader, "com/abc/resource", new Locale("de", "DE"))

  def a(implicit locale: Locale) = simpleMsgs(locale)("a").rawMsg(null)

  def b(arg0: AnyRef)(implicit locale: Locale, formatter: Markup) = simpleMsgs(locale)("b").markupMsg(Array(arg0))

  def c(arg0: AnyRef, arg1: AnyRef)(implicit locale: Locale) = simpleMsgs(locale)("c").rawMsg(Array(arg0, arg1))

  def d(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale) = simpleMsgs(locale)("d").rawMsg(Array(arg0, arg1, arg2))

  def o(implicit locale: Locale) = simpleMsgs(locale)("o").rawMsg(null)

  def t(path: String)(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale) = lookupMsgs(locale)("t").getValue(path).get.rawMsg(Array(arg0, arg1, arg2))


}

class ResourceBoilerplateTest extends FunSuite {

  implicit val markupVal = new Markup {

    val escapes: Map[Char, StringBuilder => StringBuilder] = Map(
      '<' -> ((b: StringBuilder) => b.append("&lt;")),
      '>' -> ((b: StringBuilder) => b.append("&gt;")),
      '&' -> ((b: StringBuilder) => b.append("&amp;")),
      '"' -> ((b: StringBuilder) => b.append("&quot;")),
      '\'' -> ((b: StringBuilder) => b.append("&apos;")))

    override type M = String

    override def rawMsg(string: String): String = {
      val sb = new StringBuilder
      string.foreach(c => escapes.getOrElse(c, (sb: StringBuilder) => sb.append(c))(sb))
      sb.toString
    }

    override def markupMsg(string: String): String = string
  }

  implicit val locale = new Locale("de", "DE")

  test("markup") {
    assert("&lt;&gt;" === markupVal.rawMsg("<>"))
  }

  test("simple") {
    assert(R.a === "")
    assert(R.b("x") === "<b>x</b>")
    assert(R.c("x", "y") === "<x y>")
    assert(R.d(null, null, "x") === "x")
    assert(R.o === "x")
  }

  test("lookup") {
    assert(R.t("")(null, null, null) === "1")
    assert(R.t("a")("x", "y", "z") === "z")
    assert(R.t("a.b")(null, null, null) === "3")
  }
}