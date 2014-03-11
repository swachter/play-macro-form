package eu.swdev.web.style

import scala.util.parsing.combinator.RegexParsers

object Attrs extends StyleDefs[AttrsT] {

  override def noop: AttrsT = identity
  override def result(f: Attrs => Attrs): AttrsT = f

  val empty = Map[String, Set[String]]()

  def apply(): Attrs = empty

  /**
   * Creates a set of attributes by parsing an Html fragment. Attribute values can be space separated lists of
   * strings that are stored in a set.
   *
   * @param string
   * @return
   */
  def apply(string: String): Attrs = AttrsParser.parseAll(AttrsParser.attrs, string).get

  /**
   * Creates a set of attributes that contains one attribute.
   *
   * @param attrName
   * @param value
   * @return
   */
  def apply(attrName: String, value: String*): Attrs = Map(attrName -> toSet(value))

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
      attrMap
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

/**
 * Represents an attribute with its value.
 *
 * @param attrName  The name of the attribute.
 * @param attrValue The value of the attribute. Each string in the sequence of strings may be a whitespace separated
 *                  sequence of strings by itself.
 */
case class Attr(attrName: String, attrValue: String*)
