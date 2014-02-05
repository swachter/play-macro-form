package eu.swdev.play

import scala.util.{Failure, Success, Try}

/**
  */
package object form {

  /**
   * Constructs fields for simple types and for specific type constructors
   *
   * TODO: Rename the "field2" method into "field" and remove the current field methods
   *
   * @param fieldCreator
   * @tparam M
   * @return
   */
  def field2[M](implicit fieldCreator: FieldCreator[M]) = fieldCreator.createField

  def field[V](implicit converter: SimpleConverter[V]) =
    new Field[V, Id, CState](simpleFieldConverter(converter), Constraints())

  def field[V, B[_]](implicit converter: FieldConverter[V, B]) =
    new Field[V, B, CState](converter, Constraints())

  //
  //
  //

  implicit val IntConverter = new SimpleConverter[Int] {
    def format(t: Int): String = t.toString
    def parse(s: String): Either[String, Int] = Try(Integer.parseInt(s)) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(e.getMessage)
    }
  }

  //
  //
  //

  trait FieldCreator[M] {
    type V
    type B[X]
    def createField: Field[V, B, CState]
  }

  type Id[X] = X

  //
  //
  //

  implicit def simpleFieldConverter[V](implicit converter: SimpleConverter[V]) = new FieldConverter[V, Id] {

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

    def validate(model: V, constraints: Constraints[V, _]): Seq[String] = constraints.check(model)
  }

  implicit def optionFieldConverter[V](implicit converter: SimpleConverter[V]) = new FieldConverter[V, Option] {

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

    def validate(model: Option[V], constraints: Constraints[V, _]): Seq[String] = {
      model.map(constraints.check(_)).getOrElse(Seq())
    }
  }

  implicit def seqFieldConverter[V](implicit converter: SimpleConverter[V]) = new FieldConverter[V, Seq] {

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

    def validate(model: Seq[V], constraints: Constraints[V, _]): Seq[String] = {
      model.flatMap(constraints.check(_))
    }
  }

  //
  //
  //

  implicit def simpleFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[VP] {
    type V = VP
    type B[X] = Id[X]
    def createField: Field[V, B, CState] = {
      Field[V, B, CState](simpleFieldConverter[V](simpleConverter), Constraints())
    }
  }

  implicit def optionFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Option[VP]] {
    type V = VP
    type B[X] = Option[X]
    def createField: Field[V, B, CState] = {
      Field[V, B, CState](optionFieldConverter[V](simpleConverter), Constraints())
    }
  }

  implicit def seqFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Seq[VP]] {
    type V = VP
    type B[X] = Seq[X]
    def createField: Field[V, B, CState] = {
      Field[V, B, CState](seqFieldConverter[V](simpleConverter), Constraints())
    }
  }

  //
  //
  //

  val emptyName = new Name("")

}
