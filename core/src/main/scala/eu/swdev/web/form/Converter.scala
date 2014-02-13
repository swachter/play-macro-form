package eu.swdev.web.form

/**
  */
trait SimpleConverter[V] {

  def format(t: V): String
  def parse(s: String): Either[String, V]

}

trait FieldConverter[V, B[_]] {

  def parse(view: Seq[String]): Either[Seq[String], B[V]]
  def format(model: B[V]): Seq[String]

  def validate(model: B[V], constraints: Constraints[V, _]): Seq[String]

}
