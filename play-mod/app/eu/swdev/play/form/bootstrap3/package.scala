package eu.swdev.play.form

import play.api.i18n.{Messages, Lang}
import play.api.mvc.Call
import eu.swdev.web.form._
import eu.swdev.web.style._
import scala.language.implicitConversions

import views.html.tags.eu.swdev.play.form.{ bootstrap3 => bs3 }

/**
  */
package object bootstrap3 {

  implicit class FieldRendererImpl[V, M, CS <: FieldFeatures](val fieldState: FieldState[V, M, CS])(implicit val style: Style, val lang: Lang) extends FieldRenderer[V, M, CS]

  /**
   * Allows to use the withStyle method on fields.
   *
   * @param fieldStateArg
   * @param style
   * @param langArg
   * @tparam V
   * @tparam M
   * @tparam CS
   */
  implicit class FieldWithStyle[V, M, CS <: FieldFeatures](val fieldStateArg: FieldState[V, M, CS])(implicit val style: Style, val langArg: Lang) extends WithStyle[FieldRenderer[V, M, CS]] {
    def renderer(st: Style)= new FieldRenderer[V, M, CS] {
      override def fieldState = fieldStateArg
      override implicit def lang: Lang = langArg
      override implicit def style: Style = st
    }
  }

  implicit class FormRendererImpl[M](val formState: FormState[M])(implicit val style: Style, val lang: Lang) extends FormRenderer[M]

  /**
   * Allows to use the withStyleMethod on forms.
   *
   * @param formStateArg
   * @param style
   * @param langArg
   * @tparam M
   */
  implicit class FormWithStyle[M](val formStateArg: FormState[M])(implicit val style: Style, val langArg: Lang) extends WithStyle[FormRenderer[M]] {
    def renderer(st: Style) = new FormRenderer[M] {
      override def formState: FormState[M] = formStateArg
      override implicit def lang: Lang = langArg
      override implicit def style: Style = st
    }
  }

  //
  //
  //

  implicit class FieldAttrs[V, M, CS <: FieldFeatures](val fieldState: FieldState[V, M, CS]) extends AnyVal {
    def placeholder(implicit lang: Lang): Option[Attr] = MsgLookup.placeholder(fieldState).map(Attr(placeholder_@, _))
    def labelFor(implicit style: Style): String = Bs.input.attrs(style).getOrElse("id", Set()).headOption.getOrElse(fieldState._name.toString)
    def name: String = fieldState._name.toString
  }


  implicit val callAttributeValue = new AttributeValue[Call] {
    override def asStringSet(value: Call): Set[String] = Set(value.toString())
    override def asString(value: Call): String = value.toString()
  }

  // attribute descriptions

  val class_@ = AttrDescMv("class")
  val type_@ = AttrDescSv("type")
  val min_@ = AttrDescSv("min")
  val max_@ = AttrDescSv("max")
  val step_@ = AttrDescSv("step")
  val name_@ = AttrDescSv("name")
  val value_@ = AttrDescSv("value")
  val id_@ = AttrDescSv("id")
  val action_@ = AttrDescSv("action")
  val placeholder_@ = AttrDescSv("placeholder")
  val multiple_@ = AttrDescSv("multiple")
  val for_@ = AttrDescSv("for")
  val method_@ = AttrDescSv("method")
  val checked_@ = AttrDescSv("checked")

}
