package eu.swdev.i18n

import java.util.Locale
import org.scalatest.FunSuite

/**
  */
object R {

  val resMap = ResourcesLoader.load(getClass.getClassLoader, "com/abc/resource", new Locale("de", "DE"))

  def a(implicit locale: Locale) = resMap(locale)("a").outputRaw(null)

  def b(arg0: AnyRef)(implicit locale: Locale, markup: MsgMarkup) = resMap(locale)("b").outputMarkup(Array(arg0))

  def c(arg0: AnyRef, arg1: AnyRef)(implicit locale: Locale) = resMap(locale)("c").outputRaw(Array(arg0, arg1))

  def d(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale) = resMap(locale)("d").outputRaw(Array(arg0, arg1, arg2))

  def o(implicit locale: Locale) = resMap(locale)("o").outputRaw(null)

  def t(key0: String)(arg0: AnyRef, arg1: AnyRef, arg2: AnyRef)(implicit locale: Locale) = resMap(locale)("t").lookup(key0).map(_.outputRaw(Array(arg0, arg1, arg2)))


}

class ResourceBoilerplateTest extends FunSuite {

  case class Wrapped(string: String)

  implicit val markupVal = new MsgMarkup {

    val escapes: Map[Char, StringBuilder => StringBuilder] = Map(
      '<' -> ((b: StringBuilder) => b.append("&lt;")),
      '>' -> ((b: StringBuilder) => b.append("&gt;")),
      '&' -> ((b: StringBuilder) => b.append("&amp;")),
      '"' -> ((b: StringBuilder) => b.append("&quot;")),
      '\'' -> ((b: StringBuilder) => b.append("&apos;")))

    override type M = Wrapped

    override def rawMsg(string: String): Wrapped = {
      val sb = new StringBuilder
      string.foreach(c => escapes.getOrElse(c, (sb: StringBuilder) => sb.append(c))(sb))
      Wrapped(sb.toString)
    }

    override def markupMsg(string: String): Wrapped = Wrapped(string)
  }

  implicit val locale = new Locale("de", "DE")

  test("markup") {
    assert(Wrapped("&lt;&gt;") === markupVal.rawMsg("<>"))
  }

  test("simple") {
    assert(R.a === "")
    assert(R.b("x") === Wrapped("<b>x</b>"))
    assert(R.c("x", "y") === "<x y>")
    assert(R.d(null, null, "x") === "x")
    assert(R.o === "x")
  }

  test("lookup") {
    assert(R.t("")(null, null, null) === Some("1"))
    assert(R.t("a")("x", "y", "z") === Some("z"))
    assert(R.t("a.b")(null, null, null) === Some("3"))
  }
}