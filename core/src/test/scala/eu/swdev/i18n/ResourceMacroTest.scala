package eu.swdev.i18n

import org.scalatest.FunSuite
import java.util.Locale

/**
  */
class ResourceMacroTest extends FunSuite {

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

  //

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  object ResObject

  test("object") {
    val obj = ResObject
    assert(obj.entriesMap != null)
    assert(obj.a === "")
    assert(obj.b("x") === Wrapped("<b>x</b>"))
    assert(obj.o === "x")
    assert(obj.t("")(null, null, null) === Some("1"))
    assert(obj.t("a")("x", "y", "z") === Some("z"))
    assert(obj.t("a.b")(null, null, null) === Some("3"))
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  object ResObjectWithBody {
    val abc = "abc"
  }

  test("object with body") {
    val obj = ResObjectWithBody
    assert(obj.o === "x")
    assert(obj.abc === "abc")
  }

  //

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  class ResClass

  test("class") {
    val obj = new ResClass
    assert(obj.o === "x")
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  class ResClassWithBody {
    val abc = "abc"
  }

  test("class with body") {
    val obj = new ResClassWithBody
    assert(obj.o === "x")
    assert(obj.abc === "abc")
  }

  //

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  trait ResTrait

  test("trait") {
    val obj = new ResTrait {}
    assert(obj.o === "x")
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  trait ResTraitWithBody {
    val abc = "abc"
  }

  test("trait with body") {
    val obj = new ResTraitWithBody {}
    assert(obj.o === "x")
    assert(obj.abc === "abc")
  }

  //

  trait Trait {
    def xyz = "xyz"
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  object ResObjectExtendsTrait extends Trait

  test("object extends trait") {
    val obj = ResObjectExtendsTrait
    assert(obj.o === "x")
    assert(obj.xyz === "xyz")
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  class ResClassExtendsTrait extends Trait

  test("class extends trait") {
    val obj = new ResClassExtendsTrait
    assert(obj.o === "x")
    assert(obj.xyz === "xyz")
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  class ResTraitExtendsTrait extends Trait

  test("trait extends trait") {
    val obj = new ResTraitExtendsTrait
    assert(obj.o === "x")
    assert(obj.xyz === "xyz")
  }

  //

  trait Trait2 {
    def uvw = "uvw"
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  object ResObjectExtendsTraitWithTrait extends Trait with Trait2

  test("object extends trait with trait") {
    val obj = ResObjectExtendsTraitWithTrait
    assert(obj.o === "x")
    assert(obj.xyz === "xyz")
    assert(obj.uvw === "uvw")
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  class ResClassExtendsTraitWithTrait extends Trait with Trait2

  test("class extends trait with trait") {
    val obj = new ResClassExtendsTraitWithTrait
    assert(obj.o === "x")
    assert(obj.xyz === "xyz")
    assert(obj.uvw === "uvw")
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  trait ResTraitExtendsTraitWithTrait extends Trait with Trait2

  test("trait extends trait with trait") {
    val obj = new ResTraitExtendsTraitWithTrait {}
    assert(obj.o === "x")
    assert(obj.xyz === "xyz")
    assert(obj.uvw === "uvw")
  }

  //

  trait Interface {
    def o(implicit locale: Locale, msgMarkup: MsgMarkup): AnyRef
  }

  @Resource(resourcePath = "com/abc/resource", locales = List("de_DE"))
  class ResClassExtendsInterface extends Interface

  test("resource class implements interface") {
    val obj = new ResClassExtendsInterface
    assert(obj.o === "x")
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
