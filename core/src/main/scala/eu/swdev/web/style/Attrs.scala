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
  def apply[S: AsAttrValue](attrName: String, value: S*): Attrs = Map(attrName -> value.toSet.map((x: S) => implicitly[AsAttrValue[S]].asString(x)))

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
  class AttrImpl[S: AsAttrValue](val name: String, val value: Set[String]) extends Attr
  def apply[S: AsAttrValue](attrDesc: AttrDesc, value: S*): Attr = new AttrImpl(attrDesc.attrName, attrDesc.toStringSet(value))
}

/**
 * Typeclass that allows to use values of different types as attribute values. The typeclass defines how values are
 * transformed into strings or set of strings. The transformation method used depends on the attribute kind. Some
 * attributes have a single value whereas other attributes can have a set of values (cf. AttrDesc).
 *
 * @tparam S
 */
trait AsAttrValue[S] {
  def asStringSet(value: S): Set[String]
  def asString(value: S): String
}

object AsAttrValue {
  implicit val stringAttributeValue = new AsAttrValue[String] {
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
  def toStringSet[S: AsAttrValue](s: Iterable[S]): Set[String]
}

/**
 * Attribute description for an attribute that can have multiple values.
 *
 * @param attrName
 */
case class AttrDescMv(attrName: String) extends AttrDesc {
  def toStringSet[S: AsAttrValue](s: Iterable[S]): Set[String] = {
    s.toSet.flatMap((x: S) => implicitly[AsAttrValue[S]].asStringSet(x))
  }
}

/**
 * Attribute description for an attribute that can have a single value.
 *
 * @param attrName
 */
case class AttrDescSv(attrName: String) extends AttrDesc {
  def toStringSet[S: AsAttrValue](s: Iterable[S]): Set[String] = {
    Set(s.map((x: S) => implicitly[AsAttrValue[S]].asString(x)).mkString(" "))
  }
}

/**
 * Defines a number of attribute descriptions for some well known attributes.
 */
object AttrDescs {

  val class_@ = AttrDescMv("class")
  val type_@ = AttrDescSv("type")
  val min_@ = AttrDescSv("min")
  val max_@ = AttrDescSv("max")
  val step_@ = AttrDescSv("step")
  val name_@ = AttrDescSv("name")
  val value_@ = AttrDescSv("value")
  val id_@ = AttrDescSv("id")
  val action_@ = AttrDescSv("action")
  val placeholder_@ = AttrDescSv("placeholder")
  val multiple_@ = AttrDescSv("multiple")
  val for_@ = AttrDescSv("for")
  val method_@ = AttrDescSv("method")
  val checked_@ = AttrDescSv("checked")

}
