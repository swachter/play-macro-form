package eu.swdev.web

import scala.language.implicitConversions

/**
 * The key abstractions for styling namely Attrs and Style are realized by type aliases in order to avoid boxing
 * overhead.
 */
package object style {

  /** Represents a set of attributes.
    *
    * The value of an attribute is a set of strings. The set representation was choosen because some attributes may
    * have a set of values (e.g. the Html "class" attribute).
    */
  type Attrs = Map[String, Set[String]]

  /**
   * A style contains sets of attributes. The sets of attributes are identified by keys.
   *
   * For example a style can contain the Html attributes that should be applied to different Html elements.
   */
  type Style = Map[String, Attrs]

  type AttrsT = Attrs => Attrs

  type StyleT = Style => Style

  implicit class RichStyle(val style: Style) extends AnyVal {
    def attrs(styledItem: StyledItem): Attrs = style.getOrElse(styledItem.key, Attrs.empty)
  }

  /**
   * Provide style definition operations for Attrs.
   *
   * @param attrs
   */
  implicit class RichAttrs(val attrs: Attrs) extends StyleDefs[Attrs] {
    override def noop: Attrs = attrs
    override def result(f: Attrs => Attrs): Attrs = f(attrs)
  }

  /**
   * Provide style definition operations for AttrsT.
   *
   * @param attrsT
   */
  implicit class RichAttrsT(val attrsT: AttrsT) extends StyleDefs[AttrsT] {
    override def noop: AttrsT = attrsT
    override def result(f: Attrs => Attrs): AttrsT = attrsT andThen f
  }

  /**
   * Implicit conversion that converts an Attrs into its Html representation.
   *
   * @param attrs
   * @return
   */
  implicit def attrsToString(attrs: Attrs): String = {
    (for {
      me <- attrs
    } yield {
      s"""${me._1}="${me._2.filter(!_.isEmpty).iterator.map(encodeAsHtml(_)).mkString(" ")}""""
    }).mkString(" ")
  }

  def encodeAsHtml(str: String): String = {
    if (str.exists(mustBeEscaped.contains(_))) {
      val b = new StringBuilder
      str.foreach(c => if (mustBeEscaped.contains(c)) b.append(mustBeEscaped(c)) else b.append(c))
      b.toString
    } else {
      str
    }
  }

  val mustBeEscaped = Map('\'' -> "&apos;", '"' -> "&quot;", '&' -> "&amp;", '<' -> "&lt;")
}
