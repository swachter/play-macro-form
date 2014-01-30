package eu.swdev.play.form

/**
  */
trait SimpleConverter[V] {

  def format(t: V): String
  def parse(s: String): Either[String, V]

}

trait FieldConverter[M] {

  def parse(view: Seq[String]): Either[Seq[String], M]
  def format(model: M): Seq[String]

}
