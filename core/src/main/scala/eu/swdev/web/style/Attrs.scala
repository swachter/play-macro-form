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
  def apply[S: AttributeValue](attrName: String, value: S*): Attrs = Map(attrName -> value.toSet.map((x: S) => implicitly[AttributeValue[S]].asString(x)))

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
  def name: String
  def value: Set[String]
}

object Attr {
  class AttrImpl[S: AttributeValue](val name: String, val value: Set[String]) extends Attr
  def apply[S: AttributeValue](attrDesc: AttrDesc, value: S*): Attr = new AttrImpl(attrDesc.attrName, attrDesc.toStringSet(value))
}

/**
 * Typeclass that describes how values are transformed strings or set of strings.
 *
 * @tparam S
 */
trait AttributeValue[S] {
  def asStringSet(value: S): Set[String]
  def asString(value: S): String
}

object AttributeValue {
  implicit val stringAttributeValue = new AttributeValue[String] {
    override def asStringSet(value: String): Set[String] = {
      val b = Set.newBuilder[String]
      value.split("\\s+").foreach(b += _)
      b.result
    }
    override def asString(value: String): String = value
  }
}

/**
 * Describes an attribute.
 *
 * Attribute descriptions are used to model the difference between attributes that have a single value and attributes that can
 * have a set of values. Attribute descriptions are used in style definitions when attribute values are assigned, added, removed, or defaulted.
 *
 */
trait AttrDesc {
  def attrName: String

  /**
   * Defines how an iterable of values is converted into a set of strings. Attribute descriptions for attributes  with
   * multiple values tokenize all input values and return a union set of all tokens. Attribute description for attributes
   * with a single value transform all input values into strings and return a whitespace separated string that joins
   * all these strings.
   *
   * @param s
   * @tparam S
   * @return
   */
  def toStringSet[S: AttributeValue](s: Iterable[S]): Set[String]
}

/**
 * Attribute description for an attribute that can have multiple values.
 *
 * @param attrName
 */
case class AttrDescMv(attrName: String) extends AttrDesc {
  def toStringSet[S: AttributeValue](s: Iterable[S]): Set[String] = {
    s.toSet.flatMap((x: S) => implicitly[AttributeValue[S]].asStringSet(x))
  }
}

/**
 * Attribute description for an attribute that can have a single value.
 *
 * @param attrName
 */
case class AttrDescSv(attrName: String) extends AttrDesc {
  def toStringSet[S: AttributeValue](s: Iterable[S]): Set[String] = {
    Set(s.map((x: S) => implicitly[AttributeValue[S]].asString(x)).mkString(" "))
  }
}
