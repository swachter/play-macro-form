package eu.swdev.play

import scala.util.{Failure, Success, Try}

/**
  */
package object form {

  def field[M](implicit converter: SimpleConverter[M]) = new Field(simpleFieldHandler(converter), Constraints[M, None.type, None.type, None.type](None, None, None))
  def field[M, C[_]](implicit handler: FieldHandler[M, C]) = new Field(handler, Constraints[M, None.type, None.type, None.type](None, None, None))

  def form[F <: BaseForm: FormFactory]: F = implicitly[FormFactory[F]].apply()

  def field2[V](implicit converter: SimpleConverter[V]) =
    new Field2[V, Id, None.type, None.type, None.type](simpleFieldConverter(converter), Constraints(None, None, None))

  def field2[V, B[_]](implicit converter: FieldConverter[B[V]]) =
    new Field2(converter, Constraints[V, None.type, None.type, None.type](None, None, None))



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

  implicit def simpleFieldHandler[M](implicit converter: SimpleConverter[M]) = new FieldHandler[M, Id] {


    def fillFromView(view: Seq[String], fieldState: FieldState[Id[M]]): Unit = {
      fieldState.view = view
      if (view.isEmpty) {
        fieldState.errors = Seq("missing input")
        fieldState.model = None
      } else if (!view.tail.isEmpty) {
        fieldState.errors = Seq("ambiguous input")
        fieldState.model = None
      } else {
        converter.parse(view.head) match {
          case Right(r) => {
            fieldState.errors = Seq()
            fieldState.model = Some(r)
          }
          case Left(e) => {
            fieldState.errors = Seq(e)
            fieldState.model = None
          }
        }
      }
    }

    def fillFromModel(model: Id[M], fieldState: FieldState[Id[M]]): Unit = {
      fieldState.view = Seq(converter.format(model))
      fieldState.errors = Seq()
      fieldState.model = Some(model)
    }

  }

  implicit def optionFieldHandler[M](implicit converter: SimpleConverter[M]) = new FieldHandler[M, Option] {

    def fillFromView(view: Seq[String], fieldState: FieldState[Option[M]]): Unit = {
      fieldState.view = view
      if (view.isEmpty) {
        fieldState.errors = Seq()
        fieldState.model = None
      } else if (!view.tail.isEmpty) {
        fieldState.errors = Seq("ambiguous input")
        fieldState.model = None
      } else {
        converter.parse(view.head) match {
          case Right(r) => {
            fieldState.errors = Seq()
            fieldState.model = Some(Some(r))
          }
          case Left(e) => {
            fieldState.errors = Seq(e)
            fieldState.model = None
          }
        }
      }
    }

    def fillFromModel(model: Option[M], fieldState: FieldState[Option[M]]): Unit = {
      fieldState.view = model match {
        case Some(m) => Seq(converter.format(m))
        case None => Seq()
      }
      fieldState.errors = Seq()
      fieldState.model = Some(model)
    }

  }

  implicit def seqFieldHandler[M](implicit converter: SimpleConverter[M]) = new FieldHandler[M, Seq] {

    def fillFromView(view: Seq[String], fieldState: FieldState[Seq[M]]): Unit = {
      fieldState.view = view
      val parsed = view.map(converter.parse(_))
      fieldState.errors = parsed.collect { case Left(s) => s }
      fieldState.model = Some(parsed.collect { case Right(m) => m })
    }

    def fillFromModel(model: Seq[M], fieldState: FieldState[Seq[M]]): Unit = {
      fieldState.view = model.map(converter.format(_))
      fieldState.errors = Seq()
      fieldState.model = Some(model)
    }

  }

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
