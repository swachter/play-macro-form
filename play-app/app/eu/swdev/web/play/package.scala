package eu.swdev.web

import _root_.play.api.i18n.{Messages, Lang}
import _root_.play.api.templates.Html
import eu.swdev.web.form._

import scala.language.implicitConversions
import views.html.tags.eu.swdev.play.form.bootstrap3
import eu.swdev.web.form.Set

/**
  */
package object play {

  implicit class FormRenderer[V, M, CS <: CState](val fieldState: FieldState[V, M, CS]) extends AnyVal {

    def inputText(implicit formName: Name = Name.empty, bootstrapAttrs: BootstrapAttrs = BootstrapAttrs.empty, lang: Lang): Html = {
      bootstrap3.input(fieldState, "text")
    }

    def checkBox(implicit formName: Name = Name.empty, bootstrapAttrs: BootstrapAttrs = BootstrapAttrs.empty, checkBoxValueInfo: CheckBoxValueInfo[V], lang: Lang): Html = {
      bootstrap3.checkBoxField(fieldState, checkBoxValueInfo)
    }

    def checkBoxGroup(inLineBoxes: Boolean)(implicit formName: Name = Name.empty, bootstrapAttrs: BootstrapAttrs = BootstrapAttrs.empty, lang: Lang, ev: CS <:< CState { type EN = Set; type OC = ZeroOrMore } ): Html = {
      val checkBoxes = for {
        v <- fieldState.constraints.en.get.seq
      } yield {
        val strValue = fieldState.constraints.handler.simpleConverter.format(v)
        val checked = fieldState.view.contains(strValue)
        bootstrap3.checkBox(fieldState.name.toString, strValue, checked, strValue, inLineBoxes)
      }
      bootstrap3.checkBoxOrRadioButtonGroup(fieldState, checkBoxes)
    }

    def radioButtonGroup(inLineBoxes: Boolean)(implicit formName: Name = Name.empty, bootstrapAttrs: BootstrapAttrs = BootstrapAttrs.empty, lang: Lang, ev: CS <:< CState { type EN = Set; type OC <: AtMostOne } ): Html = {
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

  object formUtil {
    def id(fieldState: FieldState[_, _, _])(implicit formName: Name): String = (formName + fieldState.name).toString
    def name(fieldState: FieldState[_, _, _])(implicit formName: Name): String = id(fieldState)
    def value(fieldState: FieldState[_, _, _]): String = fieldState.view.headOption.getOrElse("")
    def attrs(attrs: Attrs) = Html(attrs.toString)

    def label(fieldState: FieldState[_, _, _])(implicit formName: Name, lang: Lang): String = {
      def findLabel(n: Name): Option[String] = {
        val key = s"form.label.${n.toString}"
        if (Messages.isDefinedAt(key)) {
          Some(Messages(key))
        } else if (n.hasTail) {
          findLabel(n.tail)
        } else {
          None
        }
      }
      println(s"lang: $lang")
      findLabel(formName + fieldState.name).getOrElse(findLabel(fieldState.name).getOrElse((formName + fieldState.name).toString))
    }
  }

}
