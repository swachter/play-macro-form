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
  def apply[S: AttributeValue](attrName: String, value: S*): Attrs = Map(attrName -> value.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x)))

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
 */
trait Attr {
  def attrName: String
  def attrValue: Set[String]
}

object Attr {
  case class AttrImpl(attrName: String, attrValue: Set[String]) extends Attr
  def apply[S: AttributeValue](attrName: String, attrValue: S*): Attr = AttrImpl(attrName, attrValue.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].stringSet(x)))
}

/**
 * Typeclass that describes how values are transformed into attribute values.
 *
 * @tparam S
 */
trait AttributeValue[S] {
  def stringSet(value: S): Set[String]
}

object AttributeValue {
  implicit val stringAttributeValue = new AttributeValue[String] {
    override def stringSet(value: String): Set[String] = {
      val b = Set.newBuilder[String]
      value.split("\\s+").foreach(b += _)
      b.result
    }
  }
}
