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

case class BootstrapAttrs (
  form: Attrs,
  formGroup: Attrs,
  label: Attrs,
  inputDiv: Attrs,
  input: Attrs,
  button: Attrs
)
