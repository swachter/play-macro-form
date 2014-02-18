package eu.swdev.web.form

/**
  */
trait State[+M] {

  /**
   * Gets the typed value of a field or form. This method must only be called of hasFieldErrors returns false.
   *
   * @return The typed value of a field or form
   */
  def model: M

  var errors: Seq[String] = Seq()

  def addError(error: String): this.type = {
    errors = error +: errors
    this
  }

  def addErrors(errs: Seq[String]): this.type = {
    errors = errs ++ errors
    this
  }

  def hasErrors = hasFormErrors || hasFieldErrors

  def hasFormErrors: Boolean

  def hasFieldErrors: Boolean
}

trait FieldState[V, M, +CS <: CState] extends State[M] {

  /*
   * NB: The FieldState trait must not have a higher kinded type parameter because it is used as an argument
   * type in play templates. Play does not support generic template or other means to specify a higher
   * kinded argument type.
   */

  def name: Name
  def hasFormErrors = false
  def hasFieldErrors: Boolean = !errors.isEmpty
  def view: Seq[String]
  def constraints: Constraints[V, M, CS]
}

case class FieldStateWithModel[V, M, CS <: CState](name: Name, view: Seq[String], constraints: Constraints[V, M, CS], model: M) extends FieldState[V, M, CS] {
  errors = constraints.check(model)
}

case class FieldStateWithoutModel[V, M, CS <: CState](name: Name, view: Seq[String], constraints: Constraints[V, M, CS]) extends FieldState[V, M, CS] {
  def model: M = throw new NoSuchElementException(s"field does not have a model value - it contains errors: $errors")
}
