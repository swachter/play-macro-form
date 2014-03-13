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

  implicit val StringConverter = new SimpleConverterWithoutBiStateSupport[String] {
    def format(t: String): String = t
    def parse(s: String): Either[Error, String] = Right(s)
  }

  implicit val IntConverter = new SimpleConverterWithoutBiStateSupport[Int] {
    def format(t: Int): String = t.toString
    def parse(s: String): Either[Error, Int] = Try(s.toInt) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(Error("exception", e.getClass.getName, e.getMessage))
    }
  }

  implicit val DoubleConverter = new SimpleConverterWithoutBiStateSupport[Double] {
    def format(t: Double): String = t.toString
    def parse(s: String): Either[Error, Double] = Try(s.toDouble) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(Error("exception", e.getClass.getName, e.getMessage))
    }
  }

  implicit val BooleanConverter = new SimpleConverterWithBiStateSupport[Boolean] {
    def format(t: Boolean): String = t.toString
    def parse(s: String): Either[Error, Boolean] = Try(java.lang.Boolean.parseBoolean(s)) match {
      case Success(i) => Right(i)
      case Failure(e) => Left(Error("exception", e.getClass.getName, e.getMessage))
    }
    def checkedValue = true
    def uncheckedValue = false
  }

  //
  //
  //

  trait FieldCreator[M] {
    type V
    type CS <: FieldFeatures
    def createField: Field[V, M, CS]
  }

  //
  //
  //

  implicit def simpleFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[VP] {
    type V = VP
    type CS = FieldFeatures {
      type OC = ExactlyOne
      type BiV = simpleConverter.BiV
    }
    def createField: Field[V, V, CS] = {
      val handler = simpleConverter.createSimpleFieldHandler
      Field[V, V, CS](handler)
    }
  }

  implicit def optionFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Option[VP]] {
    type V = VP
    type CS = FieldFeatures {
      type OC = ZeroOrOne
      type BiV = False
    }
    def createField: Field[V, Option[VP], CS] = {
      val handler = OptionFieldHandler(simpleConverter)
      Field[V, Option[VP], CS](handler)
    }
  }

  implicit def seqFieldCreator[VP](implicit simpleConverter: SimpleConverter[VP]) = new FieldCreator[Seq[VP]] {
    type V = VP
    type CS = FieldFeatures {
      type OC = ZeroOrMore
      type BiV = False
    }
    def createField: Field[V, Seq[VP], CS] = {
      val handler = SeqFieldHandler(simpleConverter)
      Field[V, Seq[VP], CS](handler)
    }
  }

  //
  //
  //



  type Check[X] = X => Option[Error]

}
