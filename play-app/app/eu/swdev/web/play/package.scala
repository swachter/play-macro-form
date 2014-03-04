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

    def checkBox(implicit checkBoxValueInfo: CheckBoxValueInfo[V]): Html = {
      bootstrap3.checkBoxField(fieldState, checkBoxValueInfo)
    }

    def checkBoxGroup(inLineBoxes: Boolean)(implicit ev: CS <:< CState { type EN = IsSet; type OC = ZeroOrMore } ): Html = {
      val checkBoxes = for {
        v <- fieldState.constraints.en.get.seq
      } yield {
        val strValue = fieldState.constraints.handler.simpleConverter.format(v)
        val checked = fieldState.view.contains(strValue)
        bootstrap3.checkBox(fieldState.name.toString, strValue, checked, strValue, inLineBoxes)
      }
      bootstrap3.checkBoxOrRadioButtonGroup(fieldState, checkBoxes)
    }

    def radioButtonGroup(inLineBoxes: Boolean)(implicit ev: CS <:< CState { type EN = IsSet; type OC <: AtMostOne } ): Html = {
      val radioButtons = for {
        v <- fieldState.constraints.en.get.seq
      } yield {
        val strValue = fieldState.constraints.handler.simpleConverter.format(v)
        val checked = fieldState.view.contains(strValue)
        bootstrap3.radioButton(fieldState.name.toString, strValue, checked, strValue, inLineBoxes)
      }
      bootstrap3.checkBoxOrRadioButtonGroup(fieldState, radioButtons)
    }

  }

  trait FormRenderer[M] {

    implicit def style: Style
    implicit def lang: Lang
    def formState: FormState[M]

    def submit: Html = button("submit")

    def button(tpe: String = "submit"): Html = {
      val label = formUtil.findMessage(formState._name + "submit", "form.button").getOrElse(s"${formState._name}.submit")
      bootstrap3.button("submit", label)
    }

  }

  implicit class FieldRendererImpl[V, M, CS <: CState](val fieldState: FieldState[V, M, CS])(implicit val style: Style, val lang: Lang) extends FieldRenderer[V, M, CS]

  implicit class FormRendererImpl[M](val formState: FormState[M])(implicit val style: Style, val lang: Lang) extends FormRenderer[M]

  trait WithAttrs[R] {
    def withAttrs(styledItems: StyledItem*): R = {
      val transformedStyle = styledItems.foldLeft(style)((b, h) => h.transform(b))
      renderer(transformedStyle)
    }
    def style: Style
    def renderer(style: Style): R
  }

  implicit class FieldWithAttrs[V, M, CS <: CState](val fieldStateArg: FieldState[V, M, CS])(implicit val style: Style, val langArg: Lang) extends WithAttrs[FieldRenderer[V, M, CS]] {
    def renderer(st: Style)= new FieldRenderer[V, M, CS] {
      override def fieldState = fieldStateArg
      override implicit def lang: Lang = langArg
      override implicit def style: Style = st
    }
  }



  implicit class FormWithAttrs[M](val formStateArg: FormState[M])(implicit val style: Style, val langArg: Lang) extends WithAttrs[FormRenderer[M]] {
    def renderer(st: Style) = new FormRenderer[M] {
      override def formState: FormState[M] = formStateArg
      override implicit def lang: Lang = langArg
      override implicit def style: Style = st
    }
  }

  implicit class FieldAttrs[V, M, CS <: CState](val fieldState: FieldState[V, M, CS]) extends AnyVal {
    def placeholder(implicit lang: Lang): Option[Attr] = formUtil.findMessage(fieldState.name, "form.placeholder").map(Attr("placeholder", _))
    def labelFor(implicit style: Style): String = Bss.input.attrs(style).map.getOrElse("id", Set()).headOption.getOrElse(fieldState.name.toString)
    def nameForDefault(implicit style: Style): String = Bss.input.attrs(style).map.getOrElse("name", Set()).headOption.getOrElse(fieldState.name.toString) + ".default"
  }


  object formUtil {

    def label(fieldState: FieldState[_, _, _])(implicit lang: Lang): String = {
      def findLabel(name: Name): Option[String] = findMessage(name, "form.label")
      findLabel(fieldState.name).getOrElse(fieldState.name.toString)
    }

    def errors(fieldState: FieldState[_, _, _])(implicit lang: Lang): Html = {
      Html(fieldState.errors.map(e => findMessage(fieldState.name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
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
