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

  var _errors: Seq[Error] = Seq()

  def addError(error: Error): this.type = {
    _errors = error +: _errors
    this
  }

  def addErrors(errs: Seq[Error]): this.type = {
    _errors = errs ++ _errors
    this
  }

  def hasErrors = hasFormErrors || hasFieldErrors

  def hasFormErrors: Boolean

  def hasFieldErrors: Boolean

  def collectFormErrors(accu: Seq[Error]): Seq[Error]
}

trait FieldState[V, M, +CS <: CState] extends State[M] {

  /*
   * NB: The FieldState trait must not have a higher kinded type parameter because it is used as an argument
   * type in play templates. Play does not support generic templates or other means to specify a higher
   * kinded argument type.
   */

  def hasFormErrors = false
  def hasFieldErrors: Boolean = !_errors.isEmpty
  def view: Seq[String]
  def constraints: Constraints[V, M, CS]
  override def collectFormErrors(accu: Seq[Error]): Seq[Error] = accu
}

case class FieldStateWithModel[V, M, CS <: CState](_name: Name, view: Seq[String], constraints: Constraints[V, M, CS], _model: M) extends FieldState[V, M, CS] {
  _errors = constraints.check(_model)
}

case class FieldStateWithoutModel[V, M, CS <: CState](_name: Name, view: Seq[String], constraints: Constraints[V, M, CS]) extends FieldState[V, M, CS] {
  def _model: M = throw new NoSuchElementException(s"field does not have a model value - it contains errors: ${_errors}")
}

trait FormState[M] extends State[M] {

}
