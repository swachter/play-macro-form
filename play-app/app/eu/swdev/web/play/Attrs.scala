package eu.swdev.web.play

import scala.util.parsing.combinator.RegexParsers
import play.api.templates.Html

/** Represents a set of attributes.
  *
  * The value of an attribute is a set of strings. Methods allow to set, add, or remove attribute values.
  */
class Attrs(val map: Map[String, Set[String]]) {

  import Attrs._

  /**
   * Set an attribute.
   * @return
   */
  def := = new AttrsOps {
    override def doApply(attrName: String, value: Set[String]): Attrs = new Attrs(map + (attrName -> value))
  }

  /**
   * Add attribute values.
   * @return
   */
  def += = new AttrsOps {
    override def doApply(attrName: String, value: Set[String]): Attrs = new Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) ++ value)))
  }

  /**
   * Remove attribute values.
   * @return
   */
  def -= = new AttrsOps {
    override def doApply(attrName: String, value: Set[String]): Attrs = if (map.contains(attrName)) new Attrs(map + (attrName -> (map(attrName) -- value))) else Attrs.this
  }

  /**
   * Set the specified attribute if it is not already set.
   * @return
   */
  def ~= = new AttrsOps {
    override def doApply(attrName: String, value: Set[String]): Attrs = if (map.contains(attrName)) Attrs.this else new Attrs(map + (attrName -> value))
  }

  override def toString: String = {
    (for {
      me <- map
    } yield {
      s"""${me._1}="${me._2.filter(!_.isEmpty).mkString(" ")}""""
    }).mkString(" ")
  }

  abstract class AttrsOps {
    def apply(attrName: String, value: Set[String]): Attrs = doApply(attrName, value)
    def apply(attrName: String, value: String*): Attrs = doApply(attrName, value)
    def apply(check: Boolean, attrName: String, value: String*): Attrs = if (check) doApply(attrName, value) else Attrs.this
    def apply(attr: Option[Attr]): Attrs = attr match {
      case Some(a) => doApply(a.attrName, a.attrValue)
      case None => Attrs.this
    }
    def doApply(attrName: String, value: Seq[String]): Attrs = doApply(attrName, toSet(value))
    def doApply(attrName: String, value: Set[String]): Attrs
  }

}

object Attrs {

  val empty = new Attrs(Map[String, Set[String]]())

  def apply(): Attrs = empty

  /**
   * Creates a set of attributes by parsing its Html representation. Attribute values can be space separated lists of
   * strings that are stored in a set.
   *
   * @param string
   * @return
   */
  def apply(string: String): Attrs = new Attrs(AttrsParser.parseAll(AttrsParser.attrs, string).get)
  def apply(attrName: String, value: String*): Attrs = new Attrs(Map(attrName -> toSet(value)))

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
  implicit def attrsToString(attrs: Attrs): String = attrs.toString

}

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
  implicit def toString(styledItem: StyledItem)(implicit style: Style): String = styledItem.attrs(style).toString
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

