package eu.swdev.play.form

import play.api.{Play, Plugin}
import play.api.i18n.Lang
import eu.swdev.web.form.FieldState
import eu.swdev.i18n.{MsgMarkup, Resource}
import java.util.Locale
import eu.swdev.play.i18n.HtmlMarkup

/**
 * Gives access to the [[FormResourceApi]].
 */
object FormResource {
  // TODO: Better exception for missing application/plugin
  val api: FormResourceApi = Play.unsafeApplication.plugin[FormResourceApi].get
}

/**
 * Provides the messages that are used when rendering forms.
 */
trait FormResourceApi extends Plugin {
  def fieldLabel(fieldState: FieldState[_, _, _])(implicit lang: Lang): AnyRef
  def fieldPlaceholder(fieldState: FieldState[_, _, _])(implicit lang: Lang): Option[AnyRef]
}

trait StdFormResourceApi extends FormResourceApi {

  implicit def langToLocale(implicit lang: Lang): Locale = lang.toLocale
  implicit val msgMarkup = HtmlMarkup

  def fieldLabel(fieldState: FieldState[_, _, _])(implicit lang: Lang): AnyRef =
    field_label(fieldState._name).getOrElse(field_label_default(fieldState._name))

  def fieldPlaceholder(fieldState: FieldState[_, _, _])(implicit lang: Lang): Option[AnyRef] =
    field_placeholder(fieldState._name)

  // abstract methods that have to be implemented by subclasses
  // their signature is choosen in such a way that they can be implemented by the @Resource annotation
  def field_label(key: String)(implicit locale: Locale, msgMarkup: MsgMarkup): Option[AnyRef]
  def field_label_default(arg: AnyRef)(implicit locale: Locale, msgMarkup: MsgMarkup): AnyRef

  def field_placeholder(key: String)(implicit locale: Locale, msgMarkup: MsgMarkup): Option[AnyRef]

}