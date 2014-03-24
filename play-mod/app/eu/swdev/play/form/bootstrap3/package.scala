package eu.swdev.play.form

import eu.swdev.web.form.{FormState, FieldState, FieldFeatures}
import eu.swdev.web.style._
import play.api.i18n.Lang

/**
  */
package object bootstrap3 {

  implicit class FieldRendererImpl[V, M, CS <: FieldFeatures](val fieldState: FieldState[V, M, CS])(implicit val style: Style, val lang: Lang) extends BsFieldRenderer[V, M, CS]

  /**
   * Allows to use the withStyle method on fields.
   *
   * @param fieldStateArg
   * @param inStyle
   * @param langArg
   * @tparam V
   * @tparam M
   * @tparam CS
   */
  implicit class FieldWithStyle[V, M, CS <: FieldFeatures](val fieldStateArg: FieldState[V, M, CS])(implicit val inStyle: Style, val langArg: Lang) extends WithStyle[BsFieldRenderer[V, M, CS]] {
    def result(st: Style)= new BsFieldRenderer[V, M, CS] {
      override def fieldState = fieldStateArg
      override implicit def lang: Lang = langArg
      override implicit def style: Style = st
    }
  }

  implicit class FormRendererImpl[M](val formState: FormState[M])(implicit val style: Style, val lang: Lang) extends BsFormRenderer[M]

  /**
   * Allows to use the withStyleMethod on forms.
   *
   * @param formStateArg
   * @param inStyle
   * @param langArg
   * @tparam M
   */
  implicit class FormWithStyle[M](val formStateArg: FormState[M])(implicit val inStyle: Style, val langArg: Lang) extends WithStyle[BsFormRenderer[M]] {
    def result(st: Style) = new BsFormRenderer[M] {
      override def formState: FormState[M] = formStateArg
      override implicit def lang: Lang = langArg
      override implicit def style: Style = st
    }
  }

  //
  //
  //

  implicit class FieldAttrs[V, M, CS <: FieldFeatures](val fieldState: FieldState[V, M, CS]) extends AnyVal {
    def placeholder(implicit lang: Lang): Option[Attr] = MsgLookup.placeholder(fieldState).map(Attr(AttrDescs.placeholder_@, _))
    def labelFor(implicit style: Style): String = StyledItems.input.attrs(style).getOrElse("id", Set()).headOption.getOrElse(fieldState._name.toString)
    def name: String = fieldState._name.toString
  }

}
