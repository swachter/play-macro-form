package eu.swdev.play.form.bootstrap3

import eu.swdev.web.EntryPoint

/**
 * The Import object contains everything that has to be imported in order to use Bootstrap3 forms.
 * Templates typically use the import statement <code>import eu.swdev.play.form.bootstrap3.Import._</code>.
 */
object Import extends Implicits with eu.swdev.play.Implicits with eu.swdev.web.style.Implicits {

  type Style = eu.swdev.web.style.Style

  /**
   * Define a short alias for the entry point object.
   */
  val / = EntryPoint
}
