package eu.swdev.web.play

import scala.util.parsing.combinator.RegexParsers
import play.api.templates.Html

/** Supports the manipulation of a set of attributes.
  *
  * The value of an attribute is a set of strings. Methods to set, add, remove values of an attribute.
  *
  * Attrs instances can be used inside templates to augment or tweak attributes that are given as parameters from
  * the outside to templates.
  */
class Attrs(val map: Map[String, Set[String]]) extends AnyVal {

  import Attrs._

  /**
   * Sets an attribute.
   *
   * @param attrName
   * @param value
   * @return
   */
  def :=(attrName: String, value: String*) = new Attrs(map + (attrName -> toSet(value)))

  /**
   * Adds attribute values.
   *
   * @param attrName
   * @param value
   * @return
   */
  def +=(attrName: String, value: String*) = new Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) ++ toSet(value))))

  /**
   * Removes attribute values.
   *
   * @param attrName
   * @param value
   * @return
   */
  def -=(attrName: String, value: String*) = new Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) -- toSet(value))))

  /**
   * Sets the specified attribute if it is not already set.
   *
   * @param attrName
   * @param value
   * @return
   */
  def ~=(attrName: String, value: String*) = new Attrs(if (map.contains(attrName)) map else map + (attrName -> toSet(value)))

  override def toString: String = {
    (for {
      me <- map
    } yield {
      s"""${me._1}="${me._2.filter(!_.isEmpty).mkString(" ")}""""
    }).mkString(" ")
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

case class BootstrapAttrs (
  form: Attrs,
  formGroup: Attrs,
  label: Attrs,
  inputDiv: Attrs,
  input: Attrs,
  button: Attrs
)

object BootstrapAttrs {
  val empty = BootstrapAttrs(Attrs.empty, Attrs.empty, Attrs.empty, Attrs.empty, Attrs.empty, Attrs.empty)
}

