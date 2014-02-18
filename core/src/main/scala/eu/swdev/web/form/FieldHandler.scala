package eu.swdev.web.form

trait FieldConverter[V, M] {

  def parse(view: Seq[String]): Either[Seq[String], M]
  def format(model: M): Seq[String]

}

trait FieldFolder[V, M] {
  def foldField[A](m: M): A => ((A, V) => A) => A
}

/**
  */
trait FieldHandler[V, M] extends FieldConverter[V, M] with FieldFolder[V, M] {
  def simpleConverter: SimpleConverter[V]
}

case class SimpleFieldHandler[V](simpleConverter: SimpleConverter[V]) extends FieldHandler[V, V] {

  def parse(view: Seq[String]): Either[Seq[String], V] = {
    if (view.isEmpty) {
      Left(Seq("missing input"))
    } else if (!view.tail.isEmpty) {
      Left(Seq("ambiguous input"))
    } else {
      simpleConverter.parse(view.head) match {
        case Right(r) => Right(r)
        case Left(e) => Left(Seq(e))
      }
    }
  }

  def format(model: V): Seq[String] = Seq(simpleConverter.format(model))

  def foldField[A](m: V): (A) => ((A, V) => A) => A = {
    a => f => f(a, m)
  }
}

case class OptionFieldHandler[V](simpleConverter: SimpleConverter[V]) extends FieldHandler[V, Option[V]] {

  def parse(view: Seq[String]): Either[Seq[String], Option[V]] = {
    if (view.isEmpty) {
      Right(None)
    } else if (!view.tail.isEmpty) {
      Left(Seq("ambiguous input"))
    } else {
      simpleConverter.parse(view.head) match {
        case Right(r) => Right(Some(r))
        case Left(e) => Left(Seq(e))
      }
    }
  }

  def format(model: Option[V]): Seq[String] = model.map(simpleConverter.format(_)).toSeq

  def foldField[A](m: Option[V]): (A) => ((A, V) => A) => A = {
    a => m.foldLeft(a)
  }
}

case class SeqFieldHandler[V](simpleConverter: SimpleConverter[V]) extends FieldHandler[V, Seq[V]] {

  def parse(view: Seq[String]): Either[Seq[String], Seq[V]] = {
    val parsed = view.map(simpleConverter.parse(_))
    val errors = parsed.collect { case Left(s) => s }
    if (errors.isEmpty) {
      Right(parsed.collect { case Right(v) => v })
    } else {
      Left(errors)
    }
  }

  def format(model: Seq[V]): Seq[String] = model.map(simpleConverter.format(_))

  def foldField[A](m: Seq[V]): (A) => ((A, V) => A) => A = {
    a => m.foldLeft(a)
  }
}

