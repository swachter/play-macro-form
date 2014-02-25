package eu.swdev.web.play

import scala.util.parsing.combinator.RegexParsers
import play.api.templates.Html

/** Supports the manipulation of sets of attributes.
  */
class Attrs(val map: Map[String, Set[String]]) extends AnyVal {

  import Attrs._

  def :=(attrName: String, value: String*) = new Attrs(map + (attrName -> toSet(value)))
  def +=(attrName: String, value: String*) = new Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) ++ toSet(value))))
  def -=(attrName: String, value: String*) = new Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) -- toSet(value))))
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

  implicit class AttrInterpolatorX(val sc: StringContext) extends AnyVal {
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
}

case class BootstrapAttrs (
  formGroup: Attrs,
  label: Attrs,
  inputDiv: Attrs,
  input: Attrs
)

object BootstrapAttrs {
  val empty = BootstrapAttrs(Attrs.empty, Attrs.empty, Attrs.empty, Attrs.empty)
}

