package eu.swdev.web.style

import org.scalatest.FunSuite

/**
  */
class StyleTest extends FunSuite {

  val a1 = AttrDescMv("1")
  val a2 = AttrDescMv("2")

  test("Style") {
    val si = StyledItem("si")
    val style = Style(si += (a1, "a") += (a1, "b"), si -= (a1, "b"))
    assert(style("si")("1") === Set("a"))
  }

  test("StyleT") {
    val si = StyledItem("si")
    val st: StyleT = (si += (a1, "a")) andThen (si += (a2, "b"))
    val s = Style(st)
    assert(s("si")("1") === Set("a"))
    assert(s("si")("2") === Set("b"))
  }

  test ("AttrsT") {
    val at = Attrs += (a1, "a") += (a1, "b")
    val as = at(Attrs.empty)
    assert(as("1") === Set("a", "b"))
  }

}
