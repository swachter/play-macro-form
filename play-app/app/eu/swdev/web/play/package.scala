package eu.swdev.web

import _root_.play.api.i18n.{Messages, Lang}
import _root_.play.api.templates.Html
import eu.swdev.web.form._
import eu.swdev.web.style._
import scala.language.implicitConversions
import views.html.tags.eu.swdev.play.form.bootstrap3

/**
  */
package object play {

  /**
   * Provides various methods for rendering fields.
   */
  trait FieldRenderer[V, M, CS <: CState] {

    implicit def style: Style
    implicit def lang: Lang
    def fieldState: FieldState[V, M, CS]


    def inputText: Html = {
      bootstrap3.input(fieldState, "text")
    }

    def inputPassword: Html = {
      bootstrap3.input(fieldState, "password")
    }

    def checkBox(implicit checkBoxValueInfo: CheckBoxValueInfo[V]): Html = {
      bootstrap3.checkBoxField(fieldState, checkBoxValueInfo)
    }

    def checkBoxGroup(stackedNotInline: Boolean)(implicit ev: CS <:< CState { type EN = IsSet; type OC = ZeroOrMore } ): Html = {
      buttonGroup((name, value, checked, label) => bootstrap3.checkBoxOrRadioButton("checkbox", name, value, checked, label, stackedNotInline))
    }

    def radioButtonGroup(stackedNotInline: Boolean)(implicit ev: CS <:< CState { type EN = IsSet; type OC <: AtMostOne } ): Html = {
      buttonGroup((name, value, checked, label) => bootstrap3.checkBoxOrRadioButton("radio", name, value, checked, label, stackedNotInline))
    }

    def submitButtonGroup(stackedNotInline: Boolean = true)(implicit ev: CS <:< CState { type EN = IsSet } ): Html = {
      buttonGroup((name, value, checked, label) => bootstrap3.buttonCtrl("submit", label)((Bss.button += ("value", value) += ("name", name)).transform(style)))
    }

    type ButtonCreator = (String, String, Boolean, String) => Html

    def buttonGroup(creator: ButtonCreator): Html = {
      val checkBoxOrRadioButtons = for {
        v <- fieldState.field.en.get.seq
      } yield {
        val strValue = fieldState.field.handler.simpleConverter.format(v)
        val checked = fieldState.view.contains(strValue)
        creator(fieldState._name.toString, strValue, checked, strValue)
      }
      bootstrap3.checkBoxOrRadioButtonGroup(fieldState, checkBoxOrRadioButtons)
    }

    def inputRange(implicit ev1: CS <:< CState { type LB = IsSetIncl; type UB = IsSetIncl }, inputRangeStyle: InputRangeStyler[V]): Html = {
      val f = fieldState.field
      bootstrap3.input(fieldState, "range")(Bss.input.modify(inputRangeStyle(f.lb.get.value, f.ub.get.value, f.handler.simpleConverter)).transform(style), lang)
    }

  }

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
      val label = formUtil.findMessage(formState._name + "submit", "form.button").getOrElse(s"${formState._name}.submit")
      bootstrap3.button("submit", label)
    }

    def form(content: Html): Html = bootstrap3.form(formState)(content)

  }

  implicit class FieldRendererImpl[V, M, CS <: CState](val fieldState: FieldState[V, M, CS])(implicit val style: Style, val lang: Lang) extends FieldRenderer[V, M, CS]

  implicit class FormRendererImpl[M](val formState: FormState[M])(implicit val style: Style, val lang: Lang) extends FormRenderer[M]

  //
  //
  //

  /**
   * Provides the withStyle method.
   *
   * @tparam R
   */
  trait WithStyle[R] {
    /**
     * Transforms a given style and returns a renderer that uses the transformed style.
     *
     * @param styledItems
     * @return
     */
    def withStyle(styledItems: StyledItem*): R = {
      val transformedStyle = styledItems.foldLeft(style)((b, h) => h.transform(b))
      renderer(transformedStyle)
    }
    protected[this] def style: Style
    protected[this] def renderer(style: Style): R
  }

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
  implicit class FieldWithStyle[V, M, CS <: CState](val fieldStateArg: FieldState[V, M, CS])(implicit val style: Style, val langArg: Lang) extends WithStyle[FieldRenderer[V, M, CS]] {
    def renderer(st: Style)= new FieldRenderer[V, M, CS] {
      override def fieldState = fieldStateArg
      override implicit def lang: Lang = langArg
      override implicit def style: Style = st
    }
  }

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

  implicit class FieldAttrs[V, M, CS <: CState](val fieldState: FieldState[V, M, CS]) extends AnyVal {
    def placeholder(implicit lang: Lang): Option[Attr] = formUtil.findMessage(fieldState._name, "form.placeholder").map(Attr("placeholder", _))
    def labelFor(implicit style: Style): String = Bss.input.attrs(style).map.getOrElse("id", Set()).headOption.getOrElse(fieldState._name.toString)
    def nameForDefault(implicit style: Style): String = Bss.input.attrs(style).map.getOrElse("name", Set()).headOption.getOrElse(fieldState._name.toString) + ".default"
    def name: String = fieldState._name.toString
  }


  object formUtil {

    def label(fieldState: FieldState[_, _, _])(implicit lang: Lang): String = {
      def findLabel(name: Name): Option[String] = findMessage(name, "form.label")
      findLabel(fieldState._name).getOrElse(fieldState._name.toString)
    }

    def errors(formState: FormState[_])(implicit lang: Lang): Html = {
      Html(formState.collectFormErrors(Nil).map(e => findMessage(formState._name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
    }
    def errors(fieldState: FieldState[_, _, _])(implicit lang: Lang): Html = {
      Html(fieldState._errors.map(e => findMessage(fieldState._name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
    }

    def findMessage(name: Name, keyPrefix: String, args: Any*)(implicit lang: Lang): Option[String] = {
      def doFind(n: Name): Option[String] = {
        val key = s"$keyPrefix.${n.toString}"
        if (Messages.isDefinedAt(key)) {
          Some(Messages(key, args: _*))
        } else if (n.hasTail) {
          doFind(n.tail)
        } else {
          None
        }
      }
      doFind(name)
    }

  }

}
