package eu.swdev.play.form.bootstrap3

import eu.swdev.web.style._

/**
 * Provides the withStyle method.
 *
 * @tparam R The kind of renderer that is returned.
 */
trait WithStyle[R] {
  /**
   * Transforms a given style and returns a renderer that uses the transformed style.
   *
   * @param styleT A sequence of style transformations
   * @return
   */
  def withStyle(styleT: (Style => Style)*): R = {
    val transformedStyle = styleT.foldLeft(style)((b, h) => h.apply(b))
    renderer(transformedStyle)
  }
  protected[this] def style: Style
  protected[this] def renderer(style: Style): R
}

