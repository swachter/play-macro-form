package eu.swdev.web

import _root_.play.api.i18n.{Messages, Lang}
import _root_.play.api.templates.Html
import eu.swdev.web.form._

import scala.language.implicitConversions
import views.html.tags.eu.swdev.play.form.bootstrap3

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
  implicit class FieldRenderer[V, M, CS <: CState](val fieldState: FieldState[V, M, CS]) extends AnyVal {

    def inputText(implicit bootstrapAttrs: BootstrapAttrs, lang: Lang): Html = {
      bootstrap3.input(fieldState, "text")
    }

    def checkBox(implicit bootstrapAttrs: BootstrapAttrs, checkBoxValueInfo: CheckBoxValueInfo[V], lang: Lang): Html = {
      bootstrap3.checkBoxField(fieldState, checkBoxValueInfo)
    }

    def checkBoxGroup(inLineBoxes: Boolean)(implicit bootstrapAttrs: BootstrapAttrs, lang: Lang, ev: CS <:< CState { type EN = IsSet; type OC = ZeroOrMore } ): Html = {
      val checkBoxes = for {
        v <- fieldState.constraints.en.get.seq
      } yield {
        val strValue = fieldState.constraints.handler.simpleConverter.format(v)
        val checked = fieldState.view.contains(strValue)
        bootstrap3.checkBox(fieldState.name.toString, strValue, checked, strValue, inLineBoxes)
      }
      bootstrap3.checkBoxOrRadioButtonGroup(fieldState, checkBoxes)
    }

    def radioButtonGroup(inLineBoxes: Boolean)(implicit bootstrapAttrs: BootstrapAttrs, lang: Lang, ev: CS <:< CState { type EN = IsSet; type OC <: AtMostOne } ): Html = {
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
  
  implicit class FormRenderer[M](val formState: FormState[M]) {

    def submit(implicit bootstrapAttrs: BootstrapAttrs, lang: Lang): Html = button("submit")

    def button(tpe: String = "submit")(implicit bootstrapAttrs: BootstrapAttrs, lang: Lang): Html = {
      val label = formUtil.findMessage(formState._name + "submit", "form.button").getOrElse(s"${formState._name}.submit")
      bootstrap3.button("submit", label)
    }

  }

  implicit class FieldAttrs[V, M, CS <: CState](val fieldState: FieldState[V, M, CS]) extends AnyVal {
    def placeholder(implicit lang: Lang): Option[Attr] = formUtil.findMessage(fieldState.name, "form.placeholder").map(Attr("placeholder", _))
    def labelFor(implicit bsa: BootstrapAttrs): String = bsa.input.map.getOrElse("id", Set()).headOption.getOrElse(fieldState.name.toString)
    def nameForDefault(implicit bsa: BootstrapAttrs): String = bsa.input.map.getOrElse("name", Set()).headOption.getOrElse(fieldState.name.toString) + ".default"
  }


//  class OutputAttrs(val map: Map[String, Set[String]]) extends AnyVal {
//
//    def id(fieldState: FieldState[_, _, _]): Html =
//      attr("id", fieldState.name.toString)
//
//    def name(fieldState: FieldState[_, _, _]): Html =
//      attr("name", fieldState.name.toString)
//
//    def nameForDefault(fieldState: FieldState[_, _, _]): String = {
//      val n = map.getOrElse("name", Set()).headOption.getOrElse(fieldState.name.toString)
//      s"$n.default"
//    }
//
//    def value(fieldState: FieldState[_, _, _]): Html =
//      attr("value", fieldState.view.headOption.getOrElse(""))
//
//    def `type`(inputType: String): Html =
//      attr("type", inputType)
//
//    def `for`(fieldState: FieldState[_, _, _]): Html =
//      attr("for", fieldState.name.toString)
//
//    def placeholder(fieldState: FieldState[_, _, _])(implicit lang: Lang): Html =
//      attr("placeholder", formUtil.findMessage(fieldState.name, "form.placeholder").getOrElse(""))
//
//    private def attr(attrName: String, value: => String): Html = {
//      if (map.contains(attrName)) {
//        Html.empty
//      } else {
//        Html(s"""$attrName="$value"""")
//      }
//    }
//
//  }
//
//  implicit def toOutputAttrs(attrs: Attrs): OutputAttrs = new OutputAttrs(attrs.map)

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
