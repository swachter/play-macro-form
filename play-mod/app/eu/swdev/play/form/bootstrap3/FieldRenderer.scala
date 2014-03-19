package eu.swdev.play.form.bootstrap3

import eu.swdev.web.form._
import eu.swdev.web.style._
import play.api.i18n.Lang
import play.api.templates.Html
import views.html.tags.eu.swdev.play.form.{bootstrap3 => bs3}

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
    bs3.checkBox(fieldState, strValue, fieldState.equalsModel(checkedValue), MsgLookup.optValueLabel(fieldState, strValue))
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
      renderer(fieldState._name.toString, v, checked, MsgLookup.valueLabel(fieldState, strValue))
    }
  }

  private def format(v: V): String = fieldState.field.handler.simpleConverter.format(v)

}
