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

trait FieldState[+M, +C <: Constraints[_, _]] extends State[M] {
  def name: Name
  def hasFormErrors = false
  def hasFieldErrors: Boolean = !errors.isEmpty
  def view: Seq[String]
  def constraints: C
}

// a field converter is used to validate the field value during construction
// a second argument list is used for that field converter because it needs not to be part of the field state
case class FieldStateWithModel[V, B[_], C <: Constraints[V, _]](name: Name, view: Seq[String], constraints: C, model: B[V])(fc: FieldConverter[V, B]) extends FieldState[B[V], C] {
  errors = fc.validate(model, constraints)
}

case class FieldStateWithoutModel[V, B[_], C <: Constraints[V, _]](name: Name, view: Seq[String], constraints: C) extends FieldState[B[V], C] {
  def model: B[V] = throw new NoSuchElementException(s"field does not have a model value - it contains errors: $errors")
}
