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
    def parse(s: String): Either[Error, Int] = Try(Integer.parseInt(s)) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(Error("exception", s"${e.getClass.getName}: ${e.getMessage}"))
    }
  }

  implicit val BooleanConverter = new SimpleConverter[Boolean] {
    def format(t: Boolean): String = t.toString
    def parse(s: String): Either[Error, Boolean] = Try(java.lang.Boolean.parseBoolean(s)) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(Error("exception", s"${e.getClass.getName}: ${e.getMessage}"))
    }
  }

  //
  //
  //

  trait FieldCreator[M] {
    type V
    type CS <: CState
    def createField: Field[V, M, CS]
  }

  //
  //
  //

  implicit def simpleFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[VP] {
    type V = VP
    type CS = CState {
      type OC = ExactlyOne
    }
    def createField: Field[V, V, CS] = {
      val handler = SimpleFieldHandler(simpleConverter)
      Field[V, V, CS](Constraints(handler))
    }
  }

  implicit def optionFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Option[VP]] {
    type V = VP
    type CS = CState {
      type OC = ZeroOrOne
    }
    def createField: Field[V, Option[VP], CS] = {
      val handler = OptionFieldHandler(simpleConverter)
      Field[V, Option[VP], CS](Constraints(handler))
    }
  }

  implicit def seqFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Seq[VP]] {
    type V = VP
    type CS = CState {
      type OC = ZeroOrMore
    }
    def createField: Field[V, Seq[VP], CS] = {
      val handler = SeqFieldHandler(simpleConverter)
      Field[V, Seq[VP], CS](Constraints(handler))
    }
  }

  //
  //
  //



  type Check[X] = (Seq[Error], X) => Seq[Error]
}
