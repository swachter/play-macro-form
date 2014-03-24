package eu.swdev.play.form

import eu.swdev.web.style._
import play.api.i18n.Lang
import eu.swdev.web.form.FormState
import play.api.templates.Html
import views.html.tags.eu.swdev.play.form.{bootstrap3 => bs3}

/**
 * Defines methods for rendering form related things.
 */
trait FormRenderer {

  def submit: Html

  def button(tpe: String = "submit"): Html

  def form(content: Html): Html

}

