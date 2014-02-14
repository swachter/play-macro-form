package eu.swdev.web.play

import eu.swdev.web.form._
import play.api.templates.Html
import views.html.eu.swdev.play.form.bootstrap3

trait FormRenderer[M, CS <: CState] {
  
  def inputText(implicit defaultFormName: Name): Html

}

trait FormSupport {
  implicit def formRenderer[V, B[_], CS <: CState](fieldState: FieldState[V, B[V], CS]): FormRenderer[B[V], CS]
  implicit val defaultFormName = Name.empty
}

class FormSupportImpl extends FormSupport {
  implicit def formRenderer[V, B[_], CS <: CState](fieldState: FieldState[V, B[V], CS]): FormRenderer[B[V], CS] = {
    new FormRendererImpl(fieldState, this)
  }
}

class FormRendererImpl[V, B[_], CS <: CState](val fieldState: FieldState[V, B[V], CS], implicit val formSupport: FormSupport) extends FormRenderer[B[V], CS] {

  def inputText(implicit defaultFormName: Name): Html = {
    bootstrap3.inputText(fieldState)
  }
  
}