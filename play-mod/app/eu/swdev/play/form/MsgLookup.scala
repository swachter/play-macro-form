package eu.swdev.play.form

import eu.swdev.web.form.{Name, FormState, FieldState}
import play.api.i18n.{Messages, Lang}
import play.api.templates.Html

/**
 * Looks up messages for labels, visualization of values, and error messages.
 *
 * A certain message is looked by searching through a sequence keys in descending order of the specificity of keys.
 *
 * TODO: Remove after FormResourceApi is complete.
 */
object MsgLookup {

  def fieldLabel(fieldState: FieldState[_, _, _])(implicit lang: Lang): AnyRef = {
    FormResource.api.fieldLabel(fieldState)
  }

  def placeholder(fieldState: FieldState[_, _, _])(implicit lang: Lang): Option[AnyRef] = {
    FormResource.api.fieldPlaceholder(fieldState)
  }

  def valueLabel(fieldState: FieldState[_, _, _], strValue: String)(implicit lang: Lang): String = {
    optValueLabel(fieldState, strValue).getOrElse(strValue)
  }

  def optValueLabel(fieldState: FieldState[_, _, _], strValue: String)(implicit lang: Lang): Option[String] = {
    lookup(fieldState._name + strValue, "form.value")
  }

  def submitLabel(formState: FormState[_])(implicit lang: Lang): String = {
    lookup(formState._name + "submit", "form.button").getOrElse(s"${formState._name}.submit")
  }

  def errors(formState: FormState[_])(implicit lang: Lang): Html = {
    Html(formState.collectFormErrors(Nil).map(e => lookup(formState._name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
  }
  
  def errors(fieldState: FieldState[_, _, _])(implicit lang: Lang): Html = {
    Html(fieldState._errors.map(e => lookup(fieldState._name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
  }

  /**
   * Looks up a message. The specified keyPrefix and the name are joined in order to form a message key.
   * If no message is defined for that key and the name has a tail then the lookup is repeated using the tail of the
   * name.
   *
   * @param name
   * @param keyPrefix
   * @param args
   * @param lang
   * @return
   */
  private def lookup(name: Name, keyPrefix: String, args: Any*)(implicit lang: Lang): Option[String] = {
    def doLookup(n: Name): Option[String] = {
      val key = s"$keyPrefix.${n.toString}"
      if (Messages.isDefinedAt(key)) {
        Some(Messages(key, args: _*))
      } else if (n.hasTail) {
        doLookup(n.tail)
      } else {
        None
      }
    }
    doLookup(name)
  }

}

