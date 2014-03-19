package com.abc

import org.scalatest.FunSuite
import eu.swdev.web.form.BaseForm

/** Tests the boilerplate that is generated by the Form macro.
  *
  * The form objects below show the boilerplate that would have been generated by the Form macro.
  */
class BoilerplateTest extends FunSuite {

  // a form with all the boilerplate written manually
  object F extends BaseForm {

    //
    // start of boilerplate
    //

    import eu.swdev.web.form._

    /**
     * Fills a model value into a form state.
     *
     * @param model The model value that is filled into a form state.
     * @param nameArg The root name of all fields contained in the form. If the same form is rendered several times on
     *             a page then different root names must be supplied.
     * @return The resulting form state
     */
    def fill(model: FV, validationArg: Validation = WithValidation, nameArg: Name = Name("F")) = {
      FS(
        nameArg,
        f1.fill(model.f1, validationArg, nameArg + "f1"),
        f2.fill(model.f2, validationArg, nameArg + "f2"),
        f3.fill(model.f3, validationArg, nameArg + "f3"),
        f4.fill(model.f4, validationArg, nameArg + "f3")
      )(validationArg)
    }

    /**
     * Parses the string representations of field values.
     *
     * @param view The string representations of field values.
     * @param nameArg The root name of all fields contained in the form. If the same form is rendered several times on
     *             a page then different root names must be supplied.
     * @return The resulting form state.
     */
    def parse(view: Map[String, Seq[String]], validationArg: Validation = WithValidation, nameArg: Name = Name("F")) = FS(
      nameArg,
      f1.parse(view, validationArg, nameArg + "f1"),
      f2.parse(view, validationArg, nameArg + "f2"),
      f3.parse(view, validationArg, nameArg + "f3"),
      f4.parse(view, validationArg, nameArg + "f4")
    )(validationArg)

    // Define the value class that holds the typed value of the form.
    case class FV(
                  f1: Int,
                  f2: Option[Int],
                  f3: Int,
                  f4: Seq[Int]
                  )

    // Define the state class of the form.
    // The state class aggregates the states of its nested fields and forms.
    //
    // If there are any validations defined then they are called right in the constructor thereby ensuring
    // that a form state is always validated.
    case class FS(
                  _name: Name,
                  f1: FieldState[F.f1.V, F.f1.M, F.f1.F],
                  f2: FieldState[F.f2.V, F.f2.M, F.f2.F],
                  f3: FieldState[F.f3.V, F.f3.M, F.f3.F],
                  f4: FieldState[F.f4.V, F.f4.M, F.f4.F]
                  )(validationArg: Validation) extends FormState[FV] {
      val _errors = if (hasFieldErrors) Nil else validationArg.validate(Nil, Seq(validate(this)).foldLeft(List.empty[Error])((a, o) => o.map(_ :: a).getOrElse(a)))
      def hasFormErrors: Boolean = !_errors.isEmpty || Seq[State[_]](f1, f2, f3).exists(_.hasFormErrors)
      def hasFieldErrors: Boolean = Seq[State[_]](f1, f2, f3).exists(_.hasFieldErrors)
      override def _model: FV = FV(f1._model, f2._model, f3._model, f4._model)
      override def collectFormErrors(accu: Seq[Error]): Seq[Error] = Seq(f1, f2, f3).foldLeft(if (_errors.isEmpty) accu else _errors ++ accu)((a, f) => f.collectFormErrors(a))
    }

    //
    // end of boilerplate
    //

    val f1 = field[Int]
    val f2 = field[Option[Int]].lt(7)
    val f3 = field[Int].enum(Seq(3, 4, 5))
    val f4 = field[Seq[Int]].addVCheck(v => if (v % 2 == 0) Some(Error("must be odd")) else None)

    def validate(fs: FS): Option[Error] = {
      if (fs.f1._model < fs.f3._model) Some(Error("f1 must be less than f3")) else None
    }

  }

  // another form with all the boilerplate written manually
  object G extends BaseForm {

    //
    // start of boilerplate
    //

    import eu.swdev.web.form._

    def fill(model: FV, validationArg: Validation = WithValidation, name: Name = Name("G")) = FS(
      name,
      g1.fill(model.g1, validationArg, name + "g1"),
      g2.fill(model.g2, validationArg, name + "g2"),
      g3.fill(model.g3, validationArg, name + "g3")
    )(validationArg)

    def parse(view: Map[String, Seq[String]], validationArg: Validation = WithValidation, name: Name = Name("G")) = FS(
      name,
      g1.parse(view, validationArg, name + "g1"),
      g2.parse(view, validationArg, name + "g2"),
      g3.parse(view, validationArg, name + "g3")
    )(validationArg)

    case class FV(
                   g1: F.FV,
                   g2: F.FV,
                   g3: Int
                   )

    case class FS(
            _name: Name,
            g1: State[F.FV],
            g2: State[F.FV],
            g3: FieldState[G.g3.V, G.g3.M, G.g3.F]
            )(validationArg: Validation) extends FormState[FV] {
      val _errors = if (hasFieldErrors) Nil else validationArg.validate(Nil, Seq[Option[Error]]().foldLeft(List.empty[Error])((a, o) => o.map(_ :: a).getOrElse(a)))
      def hasFormErrors: Boolean = !_errors.isEmpty || Seq(g1, g2, g3).exists(_.hasFormErrors)
      def hasFieldErrors: Boolean = Seq(g1, g2, g3).exists(_.hasFieldErrors)
      override def _model: FV = FV(g1._model, g2._model, g3._model)
      override def collectFormErrors(accu: Seq[Error]): Seq[Error] = Seq(g1, g2, g3).foldLeft(if (_errors.isEmpty) accu else _errors ++ accu)((a, f) => f.collectFormErrors(a))
    }

    //
    // end of boilerplate
    //

    val g1 = F // reference another form
    val g2 = F
    val g3 = field[Int]

  }

  test("form") {

    val fs = F.fill(F.FV(1, Some(2), 3, Seq(4, 5)))

    import eu.swdev.web.form.{FieldState, FieldFeatures, IsSet}

    val simpleRenderer: FieldState[_, _, _] => String =
        state => s"simple renderer - state: $state"

    val enumRenderer: FieldState[_, _, FieldFeatures { type EN = IsSet }] => String =
        state => s"enum renderer - state: $state; enum: ${state.field.en.get}"

    val gs = G.fill(G.FV(fs._model, fs._model, 9))

  }

}

