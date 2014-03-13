package eu.swdev.web.form

/**
  */
trait State[+M] {

  def _name: Name

  /**
   * Gets the typed value of a field or form. This method must only be called of hasFieldErrors returns false.
   *
   * @return The typed value of a field or form
   */
  def _model: M

  def _errors: Seq[Error]

  def hasErrors = hasFormErrors || hasFieldErrors

  def hasFormErrors: Boolean

  def hasFieldErrors: Boolean

  def collectFormErrors(accu: Seq[Error]): Seq[Error]
}

trait FieldState[V, M, +CS <: FieldFeatures] extends State[M] {

  /*
   * NB: The FieldState trait must not have a higher kinded type parameter because it is used as an argument
   * type in play templates. Play does not support generic templates or other means to specify a higher
   * kinded argument type.
   */

  def hasFormErrors = false
  def hasFieldErrors: Boolean = !_errors.isEmpty
  def view: Seq[String]
  def field: Field[V, M, CS]
  override def collectFormErrors(accu: Seq[Error]): Seq[Error] = accu
}

case class FieldStateWithModel[V, M, CS <: FieldFeatures](_name: Name, view: Seq[String], field: Field[V, M, CS], _model: M)(validation: Validation) extends FieldState[V, M, CS] {
  val _errors = validation.validate(Nil, field.check(_model))
}

case class FieldStateWithoutModel[V, M, CS <: FieldFeatures](_name: Name, view: Seq[String], field: Field[V, M, CS], parseErrors: Seq[Error])(validation: Validation) extends FieldState[V, M, CS] {
  val _errors = validation.validate(parseErrors, Nil)
  def _model: M = throw new NoSuchElementException(s"field does not have a model value - it contains errors: ${_errors}")
}

trait FormState[M] extends State[M] {

}
