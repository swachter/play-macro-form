package eu.swdev.play.form

import play.api.i18n.{Messages, Lang}
import play.api.mvc.Call
import play.api.templates.Html
import eu.swdev.web.form._
import eu.swdev.web.style._
import scala.language.implicitConversions

import views.html.tags.eu.swdev.play.form.{ bootstrap3 => bs3 }

/**
  */
package object bootstrap3 {

  /**
   * Provides various methods for rendering fields.
   */
  trait FieldRenderer[V, M, F <: FieldFeatures] {

    implicit def style: Style
    implicit def lang: Lang
    def fieldState: FieldState[V, M, F]

    type ValueStyler = V => Style => Style
    val defaultValueStyler: ValueStyler = _ => x => x

    def inputText: Html = {
      bs3.input(fieldState, "text")
    }

    def inputPassword: Html = {
      bs3.input(fieldState, "password")
    }

    def inputRange(implicit ev1: F <:< FieldFeatures { type LB = IsSetIncl; type UB = IsSetIncl }, inputRangeStyler: InputRangeStyler[V]): Html = {
      val f = fieldState.field
      bs3.input(fieldState, "range")(Bs.input(inputRangeStyler(f.lb.get.value, f.ub.get.value, f.handler.simpleConverter))(style), lang)
    }

    /**
     * Render a check box in order to input a bi-valued field. This kind of input is only possible for bi-valued fields
     * because a single check box can distinguish only two kinds of input, namely: checked or unchecked.
     *
     * @param ev1
     * @param ev2
     * @return
     */
    def checkBox(implicit ev1: F#BiV <:< True, ev2: V =:= M): Html = {
      val checkedValue = fieldState.field.handler.checkedValue
      val strValue = format(checkedValue)
      bs3.checkBox(fieldState, strValue, fieldState.equalsModel(checkedValue), formUtil.optValueLabel(fieldState, strValue))
    }

    /**
     * Renders a group of check boxes or radio buttons.
     *
     * The decision which kind of input mechanism is used is determined by the occurrence constraint of the field. If
     * multiple values can be input then check boxes are used and radio buttons otherwise.
     *
     * @param stackedNotInline
     * @param valueStyler
     * @param ev
     * @param oc
     * @return
     */
    def selectionGroup(stackedNotInline: Boolean = true, valueStyler: ValueStyler = defaultValueStyler)(implicit ev: F <:< FieldFeatures { type EN = IsSet }, oc: OccurrenceEvidence[F#OC]): Html = {
      val inputType = if (oc.isMultiple) "checkbox" else "radio"
      bs3.selectionGroup(fieldState, enumValues((name, value, checked, label) => {
        val s: Style = valueStyler(value)(style)
        bs3.checkBoxOrRadioButton(inputType, name, format(value), checked, label, stackedNotInline)(s)
      }))
    }

    /**
     * Renders a drop down box or a multi-selection list.
     *
     * The decision which kind of input mechanism is used is determined by the occurrence constraint of the field. If
     * multiple values can be input then a multi-selection list is used and a drop down list otherwise.
     *
     * @param valueStyler
     * @param ev
     * @param oc
     * @return
     */
    def selectionList(valueStyler: ValueStyler = defaultValueStyler)(implicit ev: F <:< FieldFeatures { type EN = IsSet }, oc: OccurrenceEvidence[F#OC]): Html = {
      bs3.select(fieldState, enumValues((name, value, checked, label) => {
        val s: Style = valueStyler(value)(style)
        bs3.option(format(value), checked, label)(s)
      }), oc.isMultiple)
    }

    def submitButtonGroup(stackedNotInline: Boolean = true, valueStyler: ValueStyler = defaultValueStyler)(implicit ev: F <:< FieldFeatures { type EN = IsSet } ): Html = {
      bs3.selectionGroup(fieldState, enumValues((name, value, checked, label) => {
        val s: Style = ((Bs.button ~= (value_@, format(value)) ~= (name_@, name) += (class_@, "btn")) andThen valueStyler(value))(style)
        bs3.buttonCtrl("submit", label, stackedNotInline)(s)
      }))
    }

    type EnumValueRenderer = (String, V, Boolean, String) => Html

    private def enumValues(renderer: EnumValueRenderer): Seq[Html] = {
      for {
        v <- fieldState.field.en.get.seq
      } yield {
        val strValue = fieldState.field.handler.simpleConverter.format(v)
        val checked = fieldState.view.contains(strValue)
        renderer(fieldState._name.toString, v, checked, formUtil.valueLabel(fieldState, strValue))
      }
    }

    private def format(v: V): String = fieldState.field.handler.simpleConverter.format(v)

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
      val label = formUtil.lookupMsg(formState._name + "submit", "form.button").getOrElse(s"${formState._name}.submit")
      bs3.button("submit", label)
    }

    def form(content: Html): Html = bs3.form(formState)(content)

  }

  implicit class FieldRendererImpl[V, M, CS <: FieldFeatures](val fieldState: FieldState[V, M, CS])(implicit val style: Style, val lang: Lang) extends FieldRenderer[V, M, CS]

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
     * @param styleT A sequence of style transformations
     * @return
     */
    def withStyle(styleT: (Style => Style)*): R = {
      val transformedStyle = styleT.foldLeft(style)((b, h) => h.apply(b))
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
  implicit class FieldWithStyle[V, M, CS <: FieldFeatures](val fieldStateArg: FieldState[V, M, CS])(implicit val style: Style, val langArg: Lang) extends WithStyle[FieldRenderer[V, M, CS]] {
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

  implicit class FieldAttrs[V, M, CS <: FieldFeatures](val fieldState: FieldState[V, M, CS]) extends AnyVal {
    def placeholder(implicit lang: Lang): Option[Attr] = formUtil.lookupMsg(fieldState._name, "form.placeholder").map(Attr(placeholder_@, _))
    def labelFor(implicit style: Style): String = Bs.input.attrs(style).getOrElse("id", Set()).headOption.getOrElse(fieldState._name.toString)
    def nameForDefault(implicit style: Style): String = Bs.input.attrs(style).getOrElse("name", Set()).headOption.getOrElse(fieldState._name.toString) + ".default"
    def name: String = fieldState._name.toString
  }


  object formUtil {

    def label(fieldState: FieldState[_, _, _])(implicit lang: Lang): String = {
      lookupMsg(fieldState._name, "form.label").getOrElse(fieldState._name.toString)
    }

    def valueLabel(fieldState: FieldState[_, _, _], strValue: String)(implicit lang: Lang): String = {
      optValueLabel(fieldState, strValue).getOrElse(strValue)
    }

    def optValueLabel(fieldState: FieldState[_, _, _], strValue: String)(implicit lang: Lang): Option[String] = {
      lookupMsg(fieldState._name + strValue, "form.value")
    }

    def errors(formState: FormState[_])(implicit lang: Lang): Html = {
      Html(formState.collectFormErrors(Nil).map(e => lookupMsg(formState._name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
    }
    def errors(fieldState: FieldState[_, _, _])(implicit lang: Lang): Html = {
      Html(fieldState._errors.map(e => lookupMsg(fieldState._name + e.key, "form.error", e.args: _*).getOrElse(e.key)).mkString("<br>"))
    }

    /**
     * Lookup a message. The specified keyPrefix and name are joined in order to form  message key.
     * If no message is defined for that key and the name has a tail then the lookup is repeated with the tail of the
     * name.
     *
     * @param name
     * @param keyPrefix
     * @param args
     * @param lang
     * @return
     */
    def lookupMsg(name: Name, keyPrefix: String, args: Any*)(implicit lang: Lang): Option[String] = {
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

  implicit def zeroOrMoreOccurrenceEvidence[O <: Occurrence](implicit ev: O <:< ZeroOrMore): OccurrenceEvidence[O] = new OccurrenceEvidence[O] {
    override def isMultiple: Boolean = true
  }

  implicit def atMostOneOccurrenceEvidence[O <: Occurrence](implicit ev: O <:< AtMostOne): OccurrenceEvidence[O] = new OccurrenceEvidence[O] {
    override def isMultiple: Boolean = false
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
