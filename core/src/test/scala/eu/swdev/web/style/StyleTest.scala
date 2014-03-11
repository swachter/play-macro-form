package eu.swdev.web.style

import org.scalatest.FunSuite

/**
  */
class StyleTest extends FunSuite {

  test("Style") {
    val si = StyledItem("si")
    val style = Style(si += ("1", "a") += ("1", "b"), si -= ("1", "b"))
    assert(style("si")("1") === Set("a"))
  }

  test("StyleT") {
    val si = StyledItem("si")
    val st: StyleT = (si += ("1", "a")) andThen (si += ("2", "b"))
    val s = Style(st)
    assert(s("si")("1") === Set("a"))
    assert(s("si")("2") === Set("b"))
  }

  test ("AttrsT") {
    val at = Attrs += ("1", "a") += ("1", "b")
    val as = at(Attrs.empty)
    assert(as("1") === Set("a", "b"))
  }
}
