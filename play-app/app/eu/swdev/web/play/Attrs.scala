package eu.swdev.web.play

import scala.util.parsing.combinator.RegexParsers
import play.api.templates.Html

/**
  */
case class Attrs(map: Map[String, Set[String]]) {

  import Attrs._

  // def update(attrName: String, value: String*) = Attrs(map + (attrName -> Set(value:_*)))

  def :=(attrName: String, value: String*) = Attrs(map + (attrName -> toSet(value)))
  def +=(attrName: String, value: String*) = Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) ++ toSet(value))))
  def -=(attrName: String, value: String*) = Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) -- toSet(value))))
  def ~=(attrName: String, value: String*) = Attrs(if (map.contains(attrName)) map else map + (attrName -> toSet(value)))

  override def toString: String = {
    (for {
      me <- map
    } yield {
      s"""${me._1}="${me._2.filter(!_.isEmpty).mkString(" ")}""""
    }).mkString(" ")
  }

}

object Attrs {
  val empty = Attrs(Map[String, Set[String]]())
  def apply(): Attrs = empty
  def apply(string: String): Attrs = Attrs(AttrsParser.parseAll(AttrsParser.attrs, string).get)
  def apply(attrName: String, value: String*): Attrs = Attrs(Map(attrName -> toSet(value)))

  implicit class AttrInterpolatorX(val sc: StringContext) extends AnyVal {
    def attrs(args: Any*): Attrs = {
      val s = sc.s(args: _*)
      val attrMap: Map[String, Set[String]] = AttrsParser.parseAll(AttrsParser.attrs, s).get
      Attrs(attrMap)
    }
  }

  def toSet(value: Seq[String]): Set[String] = {
    val b = Set.newBuilder[String]
    value.foreach(_.split("\\s+").foreach(b += _))
    b.result
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

object AttrsParser extends RegexParsers {

  val attrName: Parser[String] = "[-a-zA-Z_]\\w*".r

  val attrValue: Parser[Seq[String]] = "\"" ~> rep("""[^\s"]+""".r) <~ "\""

  val attr: Parser[(String, Set[String])] = ((attrName <~ "=") ~ attrValue) ^^ ( t => (t._1, t._2.toSet))

  val attrs: Parser[Map[String, Set[String]]] = rep(attr) ^^ (Map(_: _*))
}