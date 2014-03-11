package eu.swdev.web.style

object Style {
  
  val empty = Map[String, Attrs]() //new Style(Map[String, Attrs]())

  /**
   * Creates a style by applying a sequence of style transformations to an empty style.
   *
   * @param transformations
   * @return
   */
  def apply(transformations: (Style => Style)*): Style = transformations.foldLeft(empty)((b, t) => t(b))
}

/**
 * A StyledItem represents an item that can be styled by a set of attributes (e.g. an Html element). A StyledItem
 * is identified by its key.
 *
 * A StyledItem allows to
 *
 *   - get its set of attributes from a Style
 *   - create a StyledItemT by combining it with an attribute set transformation
 *
 * @param key Identifies this StyledItem. The key is used to retrieve the corresponding set of attributes from a Style.
 */
class StyledItem(val key: String) extends AnyVal {

  def attrs(style: Style): Attrs = style.attrs(this)

  def apply(attrsT: AttrsT): StyledItemT = StyledItemT(this, attrsT :: Nil)

}

object StyledItem {

  def apply(key: String): StyledItem = new StyledItem(key)

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
  implicit def toString(styledItem: StyledItem)(implicit style: Style): String = styledItem.attrs(style)

  implicit class StyledItemStyleDefs(val styledItem: StyledItem) extends StyleDefs[StyledItemT] {
    override def noop: StyledItemT = StyledItemT(styledItem, Nil)
    override def result(f: Attrs => Attrs): StyledItemT = StyledItemT(styledItem, f :: Nil)
  }
}

/**
 * A style transformer that transforms the attributes of a specific StyledItem.
 *
 * @param styledItem
 * @param transformations
 */
class StyledItemT(val styledItem: StyledItem, val transformations: List[Attrs => Attrs]) extends StyleT {
  override def apply(v1: Style): Style = v1 + (styledItem.key -> attrs(v1)) // new Style(v1.map + (styledItem.key -> attrs(v1)))
  def attrs(v1: Style): Attrs =  transformations.foldRight(styledItem.attrs(v1))((t, attrs) => t(attrs))
  def transform(f: Attrs => Attrs) = StyledItemT(styledItem, f :: transformations)
}

object StyledItemT {

  def apply(styledItem: StyledItem, transformations: List[Attrs => Attrs]): StyledItemT = new StyledItemT(styledItem, transformations)

  implicit class StyledItemTStyleDefs(val styledItemT: StyledItemT) extends StyleDefs[StyledItemT] {
    override def noop: StyledItemT = styledItemT
    override def result(f: Attrs => Attrs): StyledItemT = styledItemT.transform(f)
  }

  /**
   * Implicit conversion that allows to use a styled item transformation as an argument to the @Html method.
   *
   * The conversion selects the corresponding attributes from an implicitly available style, transforms the attributes
   * belonging to the corresponding StyledItem, and finally converts the resulting attributes into their Html
   * representation.
   *
   * @param styledItemT
   * @param style
   * @return
   */
  implicit def toString(styledItemT: StyledItemT)(implicit style: Style): String = styledItemT.attrs(style)

}