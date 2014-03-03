package eu.swdev.web.play

import scala.util.parsing.combinator.RegexParsers
import play.api.templates.Html

/** Represents a set of attributes.
  *
  * The value of an attribute is a set of strings. The strings must not contain whitespace.
  *
  * Methods allow to set, add, remove, or specify default attribute values.
  */
class Attrs (val map: Map[String, Set[String]]) extends AnyVal {

  import Attrs._

  /**
   * Set an attribute.
   * @return
   */
  def := = new AttrsAssignOp(map)

  /**
   * Add attribute values.
   * @return
   */
  def += = new AttrsPlusOp(map)

  /**
   * Remove attribute values.
   * @return
   */
  def -= = new AttrsMinusOp(map)

  /**
   * Set the specified attribute if it is not already set.
   * @return
   */
  def ~= = new AttrsTildeOp(map)

  /**
   * Returns an Html representation of the attributes.
   *
   * @return
   */
  override def toString: String = {
    (for {
      me <- map
    } yield {
      s"""${me._1}="${me._2.filter(!_.isEmpty).mkString(" ")}""""
    }).mkString(" ")
  }

}

trait AttrsOp extends Any {

  def apply(attrName: String, value: Set[String]): Attrs = doApply(attrName, value)

  def apply(attrName: String, value: String*): Attrs = doApply(attrName, value)

  def apply(check: Boolean, attrName: String, value: String*): Attrs = if (check) doApply(attrName, value) else new Attrs(map)

  def apply(attr: Option[Attr]): Attrs = attr match {
    case Some(a) => doApply(a.attrName, a.attrValue)
    case None => new Attrs(map)
  }

  def doApply(attrName: String, value: Seq[String]): Attrs = doApply(attrName, Attrs.toSet(value))

  def doApply(attrName: String, value: Set[String]): Attrs

  def map: Map[String, Set[String]]

}

class AttrsAssignOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = new Attrs(map + (attrName -> value))
}

class AttrsPlusOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = new Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) ++ value)))
}

class AttrsMinusOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = if (map.contains(attrName)) new Attrs(map + (attrName -> (map(attrName) -- value))) else new Attrs(map)
}

class AttrsTildeOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = if (map.contains(attrName)) new Attrs(map) else new Attrs(map + (attrName -> value))
}

object Attrs {

  val empty = new Attrs(Map[String, Set[String]]())

  def apply(): Attrs = empty

  /**
   * Creates a set of attributes by parsing an Html fragment. Attribute values can be space separated lists of
   * strings that are stored in a set.
   *
   * @param string
   * @return
   */
  def apply(string: String): Attrs = new Attrs(AttrsParser.parseAll(AttrsParser.attrs, string).get)

  /**
   * Creates a set of attributes that contains one attribute.
   *
   * @param attrName
   * @param value
   * @return
   */
  def apply(attrName: String, value: String*): Attrs = new Attrs(Map(attrName -> toSet(value)))

  /**
   * String interpolator that allows to create a set of attributes from an Html fragment.
   *
   * First the specified string is interpolated using the standard string interpolator (s"..."). Then the resulting
   * Html fragment is parsed and converted into an attribute set.
   *
   * @param sc
   */
  implicit class AttrInterpolator(val sc: StringContext) extends AnyVal {
    def attrs(args: Any*): Attrs = {
      val s = sc.s(args: _*)
      val attrMap: Map[String, Set[String]] = AttrsParser.parseAll(AttrsParser.attrs, s).get
      new Attrs(attrMap)
    }
  }

  def toSet(value: Seq[String]): Set[String] = {
    val b = Set.newBuilder[String]
    value.foreach(_.split("\\s+").foreach(b += _))
    b.result
  }

  object AttrsParser extends RegexParsers {

    val attrName: Parser[String] = "[-a-zA-Z_]\\w*".r

    val attrValue: Parser[Seq[String]] = "\"" ~> rep("""[^\s"]+""".r) <~ "\""

    val attr: Parser[(String, Set[String])] = ((attrName <~ "=") ~ attrValue) ^^ ( t => (t._1, t._2.toSet))

    val attrs: Parser[Map[String, Set[String]]] = rep(attr) ^^ (Map(_: _*))
  }

  /**
   * Implicit conversion that allows to use an Attrs instance as a parameter to the @Html method.
   *
   * @param attrs
   * @return
   */
  implicit def attrsToString(attrs: Attrs): String = {
    println(s"attrs.class: ${attrs.getClass.getName}")
    attrs.toString
  }

}

/**
 * Represents an attribute with its value.
 *
 * @param attrName  The name of the attribute.
 * @param attrValue The value of the attribute. Each string in the sequence of strings may be a whitespace separated
 *                  sequence of strings by itself.
 */
case class Attr(attrName: String, attrValue: String*)

/**
 * A style contains sets of attributes. The sets of attributes are identified by keys.
 *
 * For example a style can contain the Html attributes that should be applied to different Html elements.
 * 
 * @param map
 */
case class Style(map: Map[String, Attrs]) extends AnyVal {
  def apply(key: String): Attrs = map.getOrElse(key, Attrs.empty)
}

object Style {
  val empty = Style(Map[String, Attrs]())

  /**
   * Creates a style by applying a sequence of style transformations to an empty style.
   * @param styledItem
   * @return
   */
  def apply(styledItem: StyledItem*): Style = styledItem.foldLeft(empty)((b, h) => h.transform(b))
}

/**
 * A StyledItem represents an item that is to be styled by a set of attributes (e.g. an Html element). A StyledItem
 * is identified by its key.
 *
 * A StyledItem allows to
 *
 *   - get its set of attributes from a Style
 *   - transform a Style by applying modifications to its set of attributes 
 *
 * @param key Identifies this StyledItem. The key is used to retrieve the corresponding set of attributes from a Style.
 * @param modifications Modifications that are applied to the set of attributes corresponding to this StyledItem.
 */
case class StyledItem(key: String, modifications: List[Attrs => Attrs] = Nil) {

  def attrs(style: Style): Attrs = modifications.foldRight(style(key))((m, a) => m(a))
  def transform(style: Style): Style = new Style(style.map + (key -> attrs(style)))

  def += = new Modifier {
    override def doApply(attrName: String, value: Set[String]): StyledItem = StyledItem(key, ((a: Attrs) => a.+=(attrName, value)) :: modifications)
  }
  def := = new Modifier {
    override def doApply(attrName: String, value: Set[String]): StyledItem = StyledItem(key, ((a: Attrs) => a.:=(attrName, value)) :: modifications)
  }
  def -= = new Modifier {
    override def doApply(attrName: String, value: Set[String]): StyledItem = StyledItem(key, ((a: Attrs) => a.-=(attrName, value)) :: modifications)
  }
  def ~= = new Modifier {
    override def doApply(attrName: String, value: Set[String]): StyledItem = StyledItem(key, ((a: Attrs) => a.~=(attrName, value)) :: modifications)
  }

  trait Modifier {
    def apply(attrName: String, value: String*): StyledItem = doApply(attrName, value)
    def apply(check: Boolean, attrName: String, value: String*): StyledItem = if (check) doApply(attrName, value) else StyledItem.this
    def apply(attr: Option[Attr]): StyledItem = attr match {
      case Some(a) => doApply(a.attrName, a.attrValue)
      case None => StyledItem.this
    }
    def apply(attrs: Attrs): StyledItem = {
      attrs.map.foldLeft(StyledItem.this)((h, t) => doApply(t._1, t._2))
    }
    def doApply(attrName: String, value: Seq[String]): StyledItem = doApply(attrName, Attrs.toSet(value))
    def doApply(attrName: String, value: Set[String]): StyledItem
  }

}

object StyledItem {
  /**
   * Implicit conversion that allows to use a styled item as an argument to the @Html method.
   *
   * The conversion selects the corresponding attributes from an implicitly available style and converts these attributes
   * into their Html representation.
   *
   * @param styledItem
   * @param style
   * @return
   */
  implicit def toString(styledItem: StyledItem)(implicit style: Style): String = {
    println(s"styledItem.attrs(style).class: ${styledItem.attrs(style).getClass.getName}")
    styledItem.attrs(style).toString
  }
}

/**
 * Styled items for outputting Bootstrap forms.
 */
object Bss {
  val form = StyledItem("form")
  val formGroup = StyledItem("formGroup")
  val label = StyledItem("label")
  val inputDiv = StyledItem("inputDiv")
  val input = StyledItem("input")
  val button = StyledItem("button")
}

