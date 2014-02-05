package eu.swdev.play.form

/**
  */
trait State[+M] {

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

case class FieldStateWithModel[V, B[_], C <: Constraints[V, _]](name: Name, view: Seq[String], constraints: C, model: B[V]) extends FieldState[B[V], C]

case class FieldStateWithoutModel[V, B[_], C <: Constraints[V, _]](name: Name, view: Seq[String], constraints: C) extends FieldState[B[V], C] {
  def model: B[V] = throw new NoSuchElementException(s"field does not have a model value - it contains errors: $errors")
}
