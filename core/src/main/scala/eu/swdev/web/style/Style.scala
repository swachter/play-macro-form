package eu.swdev.web.style

/**
 * A style contains sets of attributes. The sets of attributes are identified by keys.
 *
 * For example a style can contain the Html attributes that should be applied to different Html elements.
 *
 * @param map
 */
class Style(val map: Map[String, Attrs]) extends AnyVal {
  def apply(key: String): Attrs = map.getOrElse(key, Attrs.empty)
}

object Style {
  val empty = new Style(Map[String, Attrs]())

  /**
   * Creates a style by applying a sequence of style transformations to an empty style.
   * @param styledItem
   * @return
   */
  def apply(styledItem: StyledItem*): Style = styledItem.foldLeft(empty)((b, h) => h.transform(b))
}

/**
 * A StyledItem represents an item that can be styled by a set of attributes (e.g. an Html element). A StyledItem
 * is identified by its key.
 *
 * A StyledItem allows to
 *
 *   - get its set of attributes from a Style
 *   - transform a Style by applying modifications to its set of attributes
 *
 * @param key Identifies this StyledItem. The key is used to retrieve the corresponding set of attributes from a Style.
 * @param modifications Modifications that are applied to the set of attributes belonging to this StyledItem.
 */
case class StyledItem(key: String, modifications: List[Attrs => Attrs] = Nil) {

  def attrs(style: Style): Attrs = modifications.foldRight(style(key))((m, a) => m(a))
  def transform(style: Style): Style = new Style(style.map + (key -> attrs(style)))

  def modify(m: Attrs => Attrs) = StyledItem(key, m :: modifications)

  def += = new StyledItemPlusOp(this)
  def := = new StyledItemAssignOp(this)
  def -= = new StyledItemMinusOp(this)
  def ~= = new StyledItemTildeOp(this)

}

/**
 * Base trait for operations that work on a StyledItem. The trait provides several overloaded variants of the apply
 * method. Each variant allows to supply the arguments for the operation in a different way.
 *
 * NB: The trait must extends Any in order to usable as a mixin for a value class (cf. universal trait).
 */
trait StyledItemOp extends Any {
  def apply(attrName: String, value: String*): StyledItem = doApply(attrName, value)
  def apply(check: Boolean, attrName: String, value: String*): StyledItem = if (check) doApply(attrName, value) else styledItem
  def apply(attr: Option[Attr]): StyledItem = attr match {
    case Some(a) => doApply(a.attrName, a.attrValue)
    case None => styledItem
  }
  def apply(attrs: Attrs): StyledItem = {
    attrs.map.foldLeft(styledItem)((h, t) => doApply(t._1, t._2))
  }
  def doApply(attrName: String, value: Seq[String]): StyledItem = doApply(attrName, Attrs.toSet(value))
  def doApply(attrName: String, value: Set[String]): StyledItem
  def styledItem: StyledItem
}

class StyledItemAssignOp(val styledItem: StyledItem) extends AnyVal with StyledItemOp {
  override def doApply(attrName: String, value: Set[String]): StyledItem = styledItem.modify(a => a := (attrName, value))
}
class StyledItemPlusOp(val styledItem: StyledItem) extends AnyVal with StyledItemOp {
  override def doApply(attrName: String, value: Set[String]): StyledItem = styledItem.modify(a => a += (attrName, value))
}
class StyledItemMinusOp(val styledItem: StyledItem) extends AnyVal with StyledItemOp {
  override def doApply(attrName: String, value: Set[String]): StyledItem = styledItem.modify(a => a -= (attrName, value))
}
class StyledItemTildeOp(val styledItem: StyledItem) extends AnyVal with StyledItemOp {
  override def doApply(attrName: String, value: Set[String]): StyledItem = styledItem.modify(a => a ~= (attrName, value))
}

object StyledItem {
  /**
   * Implicit conversion that allows to use a styled item as an argument to the @Html method.
   *
   * The conversion selects the corresponding attributes from an implicitly available style and converts these attributes
   * into their Html representation.
   *
   * @param styledItem
   * @param style
   * @return
   */
  implicit def toString(styledItem: StyledItem)(implicit style: Style): String = styledItem.attrs(style).toString
}

