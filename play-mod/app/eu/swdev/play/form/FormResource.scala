package eu.swdev.play.form

import play.api.{Play, Plugin}
import play.api.i18n.Lang
import eu.swdev.web.form.FieldState
import eu.swdev.i18n.{MsgMarkup, Resource}
import java.util.Locale
import eu.swdev.play.i18n.HtmlMarkup

/**
  */
object FormResource {
  lazy val api = Play.unsafeApplication.plugin[FormResourceApi]
}

trait FormResourceApi extends Plugin {
  def fieldLabel(fieldState: FieldState[_, _, _])(implicit lang: Lang): AnyRef
}

trait StdFormResourceApi extends FormResourceApi {

  implicit def langToLocale(implicit lang: Lang): Locale = lang.toLocale
  implicit val msgMarkup = HtmlMarkup

  def fieldLabel(fieldState: FieldState[_, _, _])(implicit lang: Lang): AnyRef =
    field_label(fieldState._name).getOrElse(field_label_default(fieldState._name))

  def field_label(key: String)(implicit locale: Locale, msgMarkup: MsgMarkup): Option[AnyRef]
  def field_label_default(arg: AnyRef)(implicit locale: Locale, msgMarkup: MsgMarkup): AnyRef

}