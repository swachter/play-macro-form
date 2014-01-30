package eu.swdev.play.form

import scala.collection.mutable.{HashMap => MMap}

/**
  */
class FormState[F <: BaseForm](val form: F) {

  private val fieldStates = new MMap[String, FieldState[_]]

  def fieldName[M, C[_]](field: Field[M, C, _, _, _]): String = form.fieldName(field)

  def apply[M, C[_]](field: Field[M, C, _, _, _]): FieldState[C[M]] = {
    val fieldName = form.fieldName(field)
    fieldStates.get(fieldName) match {
      case Some(s) => s.asInstanceOf[FieldState[C[M]]]
      case None => {
        val newFieldState = new FieldState[C[M]]
        fieldStates(fieldName) = newFieldState
        newFieldState
      }
    }
  }

}

object FormState {

  def apply[F <: BaseForm](form: F)(v: form.Value): FormState[F] = {
    val formState = new FormState(form)
    form.set(v)(formState)
    formState
  }

  def apply[F <: BaseForm](form: F)(v: Map[String, Seq[String]]): FormState[F] = {
    val formState = new FormState(form)
    form.fill(v)(formState)
    formState
  }

}