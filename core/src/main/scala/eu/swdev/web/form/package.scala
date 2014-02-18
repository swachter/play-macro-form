package eu.swdev.web

import scala.util.{Failure, Success, Try}

/**
  */
package object form {

  /**
   * Creates fields.
   *
   * @param fieldCreator
   * @tparam M
   * @return
   */
  def field[M](implicit fieldCreator: FieldCreator[M]) = fieldCreator.createField

  //
  //
  //

  implicit val IntConverter = new SimpleConverter[Int] {
    def format(t: Int): String = t.toString
    def parse(s: String): Either[String, Int] = Try(Integer.parseInt(s)) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(s"${e.getClass.getName}: ${e.getMessage}")
    }
  }

  implicit val BooleanConverter = new SimpleConverter[Boolean] {
    def format(t: Boolean): String = t.toString
    def parse(s: String): Either[String, Boolean] = Try(java.lang.Boolean.parseBoolean(s)) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(s"${e.getClass.getName}: ${e.getMessage}")
    }
  }

  //
  //
  //

  trait FieldCreator[M] {
    type V
    def createField: Field[V, M, CState]
  }

  //
  //
  //

  implicit def simpleFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[VP] {
    type V = VP
    def createField: Field[V, V, CState] = {
      val handler = SimpleFieldHandler(simpleConverter)
      Field[V, V, CState](Constraints(handler))
    }
  }

  implicit def optionFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Option[VP]] {
    type V = VP
    def createField: Field[V, Option[VP], CState] = {
      val handler = OptionFieldHandler(simpleConverter)
      Field[V, Option[VP], CState](Constraints(handler))
    }
  }

  implicit def seqFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Seq[VP]] {
    type V = VP
    def createField: Field[V, Seq[VP], CState] = {
      val handler = SeqFieldHandler(simpleConverter)
      Field[V, Seq[VP], CState](Constraints(handler))
    }
  }

  //
  //
  //



  type Check[X] = (Seq[String], X) => Seq[String]
}
