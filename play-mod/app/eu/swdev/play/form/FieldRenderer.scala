package eu.swdev.play.form

import eu.swdev.web.form._
import eu.swdev.web.style._
import play.api.i18n.Lang
import play.api.templates.Html
import views.html.tags.eu.swdev.play.form.{bootstrap3 => bs3}

/**
 * Defines various methods for rendering form fields.
 */
trait FieldRenderer[V, M, F <: FieldFeatures] {

  /**
   * A function that returns a style transformation for a given value.
   *
   * The rendition of some form fields comprises the rendition of a collection of HTML fragments that corresponding to
   * the possible values of that field. For example the rendition of a select box comprises the rendition of its options.
   * A value styler allows to use different styles for each value.
   */
  type ValueStyler = V => StyleT

  /**
   * The default value styler does not modify the given style.
   */
  val defaultValueStyler: ValueStyler = _ => identity

  def inputText: Html

  def inputPassword: Html

  def inputRange(implicit ev1: F <:< FieldFeatures { type LB = IsSetIncl; type UB = IsSetIncl }, inputRangeStyler: InputRangeStyler[V]): Html

  /**
   * Render a check box in order to input a bi-valued field. This kind of input is only possible for bi-valued fields
   * because a single check box can distinguish only two kinds of input, namely: checked or unchecked.
   *
   * @param ev1
   * @param ev2
   * @return
   */
  def checkBox(implicit ev1: F#BiV <:< True, ev2: V =:= M): Html

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
  def selectionGroup(stackedNotInline: Boolean = true, valueStyler: ValueStyler = defaultValueStyler)(implicit ev: F <:< FieldFeatures { type EN = IsSet }, oc: OccurrenceEvidence[F#OC]): Html

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
  def selectionList(valueStyler: ValueStyler = defaultValueStyler)(implicit ev: F <:< FieldFeatures { type EN = IsSet }, oc: OccurrenceEvidence[F#OC]): Html

  def submitButtonGroup(stackedNotInline: Boolean = true, valueStyler: ValueStyler = defaultValueStyler)(implicit ev: F <:< FieldFeatures { type EN = IsSet } ): Html

}
