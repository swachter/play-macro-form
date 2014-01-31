package eu.swdev.play

import scala.util.{Failure, Success, Try}

/**
  */
package object form {

  def field[V](implicit converter: SimpleConverter[V]) =
    new Field[V, Id, CState](simpleFieldConverter(converter), Constraints())

  def field[V, B[_]](implicit converter: FieldConverter[B[V]]) =
    new Field[V, B, CState](converter, Constraints())

  implicit val IntConverter = new SimpleConverter[Int] {
    def format(t: Int): String = t.toString
    def parse(s: String): Either[String, Int] = Try(Integer.parseInt(s)) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(e.getMessage)
    }
  }

  type Id[X] = X

  //
  //
  //

  implicit def simpleFieldConverter[V](implicit converter: SimpleConverter[V]) = new FieldConverter[V] {


    def parse(view: Seq[String]): Either[Seq[String], V] = {
      if (view.isEmpty) {
        Left(Seq("missing input"))
      } else if (!view.tail.isEmpty) {
        Left(Seq("ambiguous input"))
      } else {
        converter.parse(view.head) match {
          case Right(r) => Right(r)
          case Left(e) => Left(Seq(e))
        }
      }
    }

    def format(model: V): Seq[String] = Seq(converter.format(model))

  }

  implicit def optionFieldConverter[V](implicit converter: SimpleConverter[V]) = new FieldConverter[Option[V]] {

    def parse(view: Seq[String]): Either[Seq[String], Option[V]] = {
      if (view.isEmpty) {
        Right(None)
      } else if (!view.tail.isEmpty) {
        Left(Seq("ambiguous input"))
      } else {
        converter.parse(view.head) match {
          case Right(r) => Right(Some(r))
          case Left(e) => Left(Seq(e))
        }
      }
    }

    def format(model: Option[V]): Seq[String] = model.map(converter.format(_)).toSeq

  }

  implicit def seqFieldConverter[V](implicit converter: SimpleConverter[V]) = new FieldConverter[Seq[V]] {

    def parse(view: Seq[String]): Either[Seq[String], Seq[V]] = {
      val parsed = view.map(converter.parse(_))
      val errors = parsed.collect { case Left(s) => s }
      if (errors.isEmpty) {
        Right(parsed.collect { case Right(v) => v })
      } else {
        Left(errors)
      }
    }

    def format(model: Seq[V]): Seq[String] = model.map(converter.format(_))

  }

}
