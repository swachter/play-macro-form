package eu.swdev.web.style


/**
 * Provides the withStyle method.
 *
 * The withStyle allows the transformation of a style and uses that style in order to calculate a result. The
 * withStyle method is meant to be used in method chains.
 *
 * @tparam R The kind of result that is returned.
 */
trait WithStyle[R] {

  /**
   * Transforms a given style and returns a result that uses the transformed style.
   *
   * @param styleT A sequence of style transformations
   * @return
   */
  def withStyle(styleT: StyleT*): R = {
    val outStyle = styleT.foldLeft(inStyle)((b, h) => h(b))
    result(outStyle)
  }

  protected[this] def inStyle: Style
  protected[this] def result(outStyle: Style): R
}

