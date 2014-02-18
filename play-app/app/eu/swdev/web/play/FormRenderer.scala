package eu.swdev.web.play

import eu.swdev.web.form._
import play.api.templates.Html
import views.html.tags.eu.swdev.play.form.bootstrap3

trait FormRenderer[V, M, CS <: CState] {
  
  def inputText(implicit defaultFormName: Name): Html
  def checkBox(implicit defaultFormName: Name, checkBoxValueInfo: CheckBoxValueInfo[V]): Html

  def checkBoxes(implicit defaultFormName: Name, ev: CS <:< CState { type EN = Set } ): Html

}

trait FormSupport {
  implicit def formRenderer[V, M, CS <: CState](fieldState: FieldState[V, M, CS]): FormRenderer[V, M, CS]
  implicit val defaultFormName = Name.empty
  implicit val booleanCheckBoxValueInfo = new CheckBoxValueInfo[Boolean] {
    override def checkedValue: String = "true"
    override def uncheckedValue: String = "false"
  }
}

class FormSupportImpl extends FormSupport {
  implicit def formRenderer[V, M, CS <: CState](fieldState: FieldState[V, M, CS]): FormRenderer[V, M, CS] = {
    new FormRendererImpl(fieldState, this)
  }
}

class FormRendererImpl[V, M, CS <: CState](val fieldState: FieldState[V, M, CS], implicit val formSupport: FormSupport) extends FormRenderer[V, M, CS] {

  def inputText(implicit defaultFormName: Name): Html = {
    bootstrap3.inputText(fieldState)
  }

  def checkBox(implicit defaultFormName: Name, checkBoxValueInfo: CheckBoxValueInfo[V]): Html = {
    bootstrap3.checkBoxField(fieldState, checkBoxValueInfo)
  }

  def checkBoxes(implicit defaultFormName: Name, ev: CS <:< CState { type EN = Set } ): Html
}

trait CheckBoxValueInfo[V] {
  def uncheckedValue: String
  def checkedValue: String
  def isChecked(view: Seq[String]): Boolean = view.contains(checkedValue)
}