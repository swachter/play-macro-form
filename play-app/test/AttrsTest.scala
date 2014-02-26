import eu.swdev.web.play.Attrs
import org.scalatest.{Inside, FunSuite}

/**
  */
class AttrsTest extends FunSuite with Inside {

  test("attrs") {
    val a1 = Attrs.empty += ("name", "value") += ("class", "c1", "c2")
    assert(a1.map("name") === Set("value"))
    assert(a1.map("class") === Set("c1", "c2"))
  }

  test("attrs parser") {
    import eu.swdev.web.play._
    import Attrs._

    inside(AttrsParser.parseAll(AttrsParser.attrName, """name""")) { case AttrsParser.Success("name", _) => }
    inside(AttrsParser.parseAll(AttrsParser.attrValue, """"a b c"""")) { case AttrsParser.Success(Seq("a", "b", "c"), _) => }
    inside(AttrsParser.parseAll(AttrsParser.attr, """name="abc"""")) { case AttrsParser.Success(("name", set), _) if set == Set("abc") => }

    val as = attrs""" name="abc" """

    assert(as.map("name").contains("abc"))

  }
}
