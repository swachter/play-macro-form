package eu.swdev.play.form

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

  def addError(error: String): Unit = {
    errors = error +: errors
  }

  def hasErrors = hasFormErrors || hasFieldErrors

  def hasFormErrors: Boolean

  def hasFieldErrors: Boolean
}

trait FieldState[V, B[_], +CS <: CState] extends State[B[V]] {

  def name: Name
  def hasFormErrors = false
  def hasFieldErrors: Boolean = !errors.isEmpty
  def view: Seq[String]
  def constraints: Constraints[V, CS]
}

// a field converter is used to validate the field value during construction
// a second argument list is used for that field converter because it needs not to be part of the field state
case class FieldStateWithModel[V, B[_], CS <: CState](name: Name, view: Seq[String], constraints: Constraints[V, CS], model: B[V])(fc: FieldConverter[V, B]) extends FieldState[V, B, CS] {
  errors = fc.validate(model, constraints)
}

case class FieldStateWithoutModel[V, B[_], CS <: CState](name: Name, view: Seq[String], constraints: Constraints[V, CS]) extends FieldState[V, B, CS] {
  def model: B[V] = throw new NoSuchElementException(s"field does not have a model value - it contains errors: $errors")
}
