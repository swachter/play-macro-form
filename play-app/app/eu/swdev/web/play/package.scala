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

  /**
   * Provides various methods for rendering fields.
   *
   * @param fieldState
   * @tparam V
   * @tparam M
   * @tparam CS
   */
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
      def findLabel(name: Name): Option[String] = findMessage(name, "form.label")
      findLabel(formName + fieldState.name).getOrElse(findLabel(fieldState.name).getOrElse((formName + fieldState.name).toString))
    }

    def errors(fieldState: FieldState[_, _, _])(implicit lang: Lang): Html = {
      Html(fieldState.errors.map(e => findMessage(fieldState.name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
    }

    def placeholder(fieldState: FieldState[_, _, _])(implicit lang: Lang): Html = {
      def findLabel(name: Name): Option[String] = findMessage(name, "form.label")
      Html(findMessage(fieldState.name, "form.placeholder").map(p => s"""placeholder="$p""""").getOrElse(""))
    }

    private def findMessage(name: Name, keyPrefix: String, args: Any*)(implicit lang: Lang): Option[String] = {
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
