package eu.swdev.i18n

import org.scalatest.FunSuite
import java.util.Locale

/**
  */
class ResourceMacroTest extends FunSuite {

  @Resource(resourcePath = "com/abc/resource")
  object R {

  }

  case class Wrapped(string: String)

  implicit val markupVal = new Markup {

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

  test("simple") {
    assert(R.simpleMsgs != null)
    assert(R.lookupMsgs != null)
    assert(R.a === "")
    assert(R.b("x") === Wrapped("<b>x</b>"))
    assert(R.o === "x")

  }

  test("lookup") {
    assert(R.t("")(null, null, null) === Some("1"))
    assert(R.t("a")("x", "y", "z") === Some("z"))
    assert(R.t("a.b")(null, null, null) === Some("3"))
  }

}

//Apply(
//  Select(
//    Apply(
//      Select(
//        New(
//          Ident(
//            newTypeName("CompiledMessages")
//          )
//        ),
//        nme.CONSTRUCTOR
//      ),
//      List(
//        AssignOrNamedArg(
//          Ident(newTermName("resourcePath")),
//          Literal(Constant("abc"))
//        )
//      )
//    ),
//    newTermName("macroTransform")
//  ),
//  List(ModuleDef(Modifiers(), newTermName("Msg"), Template(List(Select(Ident(scala), newTypeName("AnyRef"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))))))
//)
//
