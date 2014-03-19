package eu.swdev.play.form.bootstrap3

import eu.swdev.web.style._
import play.api.i18n.Lang
import eu.swdev.web.form.FormState
import play.api.templates.Html
import views.html.tags.eu.swdev.play.form.{bootstrap3 => bs3}

/**
 * Provides methods for rendering form related things.
 *
 * @tparam M
 */
trait FormRenderer[M] {

  implicit def style: Style
  implicit def lang: Lang
  def formState: FormState[M]

  def submit: Html = button("submit")

  def button(tpe: String = "submit"): Html = {
    bs3.button("submit", MsgLookup.submitLabel(formState))
  }

  def form(content: Html): Html = bs3.form(formState)(content)

}

