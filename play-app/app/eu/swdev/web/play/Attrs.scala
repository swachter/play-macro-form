package eu.swdev.web.play

import scala.util.parsing.combinator.RegexParsers
import play.api.templates.Html

/** Represents a set of attributes.
  *
  * The value of an attribute is a set of strings. Methods allow to set, add, or remove attribute values.
  *
  * Attrs instances can be used inside templates to augment or tweak attributes that are given as template parameters.
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
 * A style defines attributes for various items.
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
   * Creates a style by applying a sequence of styles to an empty style.
   * @param stylers
   * @return
   */
  def apply(stylers: Styler*): Style = stylers.foldLeft(empty)((b, h) => h.transform(b))
}

/**
 * A styler is responsible for a certain styled item. It allows to apply style modifications and to access the
 * resulting attributes.
 *
 * @param key A key that identifies a certain styled item (e.g. a specific Html element).
 * @param modifications A list of modifications that is applied to a style.
 */
case class Styler(key: String, modifications: List[Attrs => Attrs] = Nil) {

  def select(s: Style): Attrs = modifications.foldRight(s(key))((m, a) => m(a))
  def transform(s: Style): Style = new Style(s.map + (key -> select(s)))

  def += = new Modifier {
    override def doApply(attrName: String, value: Set[String]): Styler = Styler(key, ((a: Attrs) => a.+=(attrName, value)) :: modifications)
  }
  def := = new Modifier {
    override def doApply(attrName: String, value: Set[String]): Styler = Styler(key, ((a: Attrs) => a.:=(attrName, value)) :: modifications)
  }
  def -= = new Modifier {
    override def doApply(attrName: String, value: Set[String]): Styler = Styler(key, ((a: Attrs) => a.-=(attrName, value)) :: modifications)
  }
  def ~= = new Modifier {
    override def doApply(attrName: String, value: Set[String]): Styler = Styler(key, ((a: Attrs) => a.~=(attrName, value)) :: modifications)
  }

  trait Modifier {
    def apply(attrName: String, value: String*): Styler = doApply(attrName, value)
    def apply(check: Boolean, attrName: String, value: String*): Styler = if (check) doApply(attrName, value) else Styler.this
    def apply(attr: Option[Attr]): Styler = attr match {
      case Some(a) => doApply(a.attrName, a.attrValue)
      case None => Styler.this
    }
    def apply(attrs: Attrs): Styler = {
      attrs.map.foldLeft(Styler.this)((h, t) => doApply(t._1, t._2))
    }
    def doApply(attrName: String, value: Seq[String]): Styler = doApply(attrName, Attrs.toSet(value))
    def doApply(attrName: String, value: Set[String]): Styler
  }

}

object Styler {
  /**
   * Implicit conversion that allows to use an styler as an argument to the @Html method.
   *
   * The conversion selects the corresponding attributes from an implicitly available style and converts these attributes
   * into their Html representation.
   *
   * @param styler
   * @param style
   * @return
   */
  implicit def toString(styler: Styler)(implicit style: Style): String = styler.select(style).toString
}

/**
 * Bootstrap stylers
 */
object Bss {
  val form = Styler("form")
  val formGroup = Styler("formGroup")
  val label = Styler("label")
  val inputDiv = Styler("inputDiv")
  val input = Styler("input")
  val button = Styler("button")
}

