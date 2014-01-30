package eu.swdev.play.form

/**
  */
trait State[M] {

  def model: M

  var errors: Seq[String] = Seq()

  def addError(error: String): Unit = error :+ errors

  def hasErrors: Boolean
}

trait FieldState2[M, C <: Constraints[_, _, _, _]] extends State[M] {
  def name: Name
  def hasErrors: Boolean = !errors.isEmpty
  def view: Seq[String]
  def constraints: C
}

case class FieldStateWithModel[V, B[_], C <: Constraints[V, _, _, _]](name: Name, view: Seq[String], constraints: C, model: B[V]) extends FieldState2[B[V], C]

case class FieldStateWithoutModel[V, B[_], C <: Constraints[V, _, _, _]](name: Name, view: Seq[String], constraints: C) extends FieldState2[B[V], C] {
  def model: B[V] = throw new NoSuchElementException(s"field does not have a model value - it contains errors: $errors")
}