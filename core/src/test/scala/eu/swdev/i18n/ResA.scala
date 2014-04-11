package eu.swdev.i18n

import org.scalatest.FunSuite
import java.util.Locale
import scala.language.implicitConversions

/**
  */
class ResA extends FunSuite {

  @Resource(resourcePath = "com/abc/res-a", locales = List("en_US", "en_UK", "de_DE"))
  object R

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

  val de_DE = new Locale("de", "DE")
  val en_US = new Locale("en", "US")
  val en_UK = new Locale("en", "UK")

  test("mixed markup and raw") {
    assert(R.oneAndOne(de_DE, markupVal) === Wrapped("<b>1&amp;</b>"))
    assert(R.oneAndOne(en_US, markupVal) === Wrapped("1&amp;1"))
  }

  test("aggregation") {
    assert(R.color(de_DE, markupVal) === "Farbe")
    assert(R.color(en_US, markupVal) === "color")
    assert(R.color(en_UK, markupVal) === "colour")
  }

  test("override") {
    assert(R.override_(de_DE, markupVal) === "de")
    assert(R.override_(en_US, markupVal) === "en_US")
    assert(R.override_(en_UK, markupVal) === "en_UK")
  }

  test("nested lookup") {
    implicit val locale = de_DE
    implicit def intToString(i: Int): String = i.toString
    assert(R.map3(0, 0, 0) === Some("0"))
    assert(R.map3(1, 0, 0) === Some("0"))
    assert(R.map3(2, 0, 0) === Some("0"))
    assert(R.map3(0, 1, 0) === Some("0"))
    assert(R.map3(1, 1, 0) === Some("0"))
    assert(R.map3(2, 1, 0) === Some("0"))
    assert(R.map3(0, 2, 0) === Some("0"))
    assert(R.map3(1, 2, 0) === Some("0"))
    assert(R.map3(2, 2, 0) === Some("0"))
    assert(R.map3(0, 0, 1) === Some("1"))
    assert(R.map3(1, 0, 1) === Some("1"))
    assert(R.map3(2, 0, 1) === Some("1"))
    assert(R.map3(0, 1, 1) === Some("1"))
    assert(R.map3(1, 1, 1) === Some("1"))
    assert(R.map3(2, 1, 1) === Some("1"))
    assert(R.map3(0, 2, 1) === Some("1"))
    assert(R.map3(1, 2, 1) === Some("1"))
    assert(R.map3(2, 2, 1) === Some("1"))
  }
}
