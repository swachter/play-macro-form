package eu.swdev.web.style

import scala.util.parsing.combinator.RegexParsers

/** Represents a set of attributes.
  *
  * The value of an attribute is a set of strings. The strings must not contain whitespace.
  *
  * Methods allow to set, add, remove, or specify default attribute values.
  */
class Attrs (val map: Map[String, Set[String]]) extends AnyVal {

  import Attrs._

  /**
   * Set an attribute.
   * @return
   */
  def := = new AttrsAssignOp(map)

  /**
   * Add attribute values.
   * @return
   */
  def += = new AttrsPlusOp(map)

  /**
   * Remove attribute values.
   * @return
   */
  def -= = new AttrsMinusOp(map)

  /**
   * Set the specified attribute if it is not already set.
   * @return
   */
  def ~= = new AttrsTildeOp(map)

  /**
   * Returns an Html representation of the attributes.
   *
   * @return
   */
  override def toString: String = {
    (for {
      me <- map
    } yield {
      s"""${me._1}="${me._2.filter(!_.isEmpty).mkString(" ")}""""
    }).mkString(" ")
  }

}

/**
 * Base trait for operations that work on a set of attributes. The trait provides several overloaded variants of the apply
 * method. Each variant allows to supply the arguments for the operation in a different way.
 *
 * NB: The trait must extends Any in order to usable as a mixin for a value class (cf. universal trait).
 */
trait AttrsOp extends Any {

  def apply(attrName: String, value: Set[String]): Attrs = doApply(attrName, value)

  def apply(attrName: String, value: String*): Attrs = doApply(attrName, value)

  def apply(check: Boolean, attrName: String, value: String*): Attrs = if (check) doApply(attrName, value) else new Attrs(map)

  def apply(attr: Option[Attr]): Attrs = attr match {
    case Some(a) => doApply(a.attrName, a.attrValue)
    case None => new Attrs(map)
  }

  def doApply(attrName: String, value: Seq[String]): Attrs = doApply(attrName, Attrs.toSet(value))

  def doApply(attrName: String, value: Set[String]): Attrs

  def map: Map[String, Set[String]]

}

class AttrsAssignOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = new Attrs(map + (attrName -> value))
}

class AttrsPlusOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = new Attrs(map + (attrName -> (map.getOrElse(attrName, Set()) ++ value)))
}

class AttrsMinusOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = if (map.contains(attrName)) new Attrs(map + (attrName -> (map(attrName) -- value))) else new Attrs(map)
}

class AttrsTildeOp(val map: Map[String, Set[String]]) extends AnyVal with AttrsOp {
  override def doApply(attrName: String, value: Set[String]): Attrs = if (map.contains(attrName)) new Attrs(map) else new Attrs(map + (attrName -> value))
}

object Attrs {

  val empty = new Attrs(Map[String, Set[String]]())

  def apply(): Attrs = empty

  /**
   * Creates a set of attributes by parsing an Html fragment. Attribute values can be space separated lists of
   * strings that are stored in a set.
   *
   * @param string
   * @return
   */
  def apply(string: String): Attrs = new Attrs(AttrsParser.parseAll(AttrsParser.attrs, string).get)

  /**
   * Creates a set of attributes that contains one attribute.
   *
   * @param attrName
   * @param value
   * @return
   */
  def apply(attrName: String, value: String*): Attrs = new Attrs(Map(attrName -> toSet(value)))

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

/**
 * Represents an attribute with its value.
 *
 * @param attrName  The name of the attribute.
 * @param attrValue The value of the attribute. Each string in the sequence of strings may be a whitespace separated
 *                  sequence of strings by itself.
 */
case class Attr(attrName: String, attrValue: String*)
