package com.abc

import eu.swdev.web.form.{Name, Error, Form}
import org.scalatest.FunSuite
import scala.Error

/** Tests the Form macro annotation
  */
class MacroTest extends FunSuite {

  @Form
  object F {
    val f1 = field[Int].lt(5).enum(Seq(2,3,4))
    val f2 = field[Option[Int]]
    val f3 = field[Seq[Int]]

    // method is called when a FormState is constructed
    def test(fs: FS): Unit = {
      if (fs.f1._model % 2 == 0) {
        fs.f1.addError(Error("odd"))
      }
    }
  }

  @Form
  object G {
    val g1 = F
    val g2 = field[Int]
    val g3 = F

    def validate(fs: FS): Unit = {
      if (fs.g2._model % 2 == 1) {
        fs.g2.addError(Error("even"))
      }
    }
  }

  @Form
  object H {
    val f1 = field[Int]
  }

  test("filling of forms") {
    val f = F.FV(2, Some(4), Seq(1, 2))
    val g = G.FV(f, 4, f)

    val fs = F.fill(f)
    assert(fs.f1.hasFieldErrors === true)
    val fsm = fs._model
    assert(f === fsm)

    val gs = G.fill(g)
    assert(gs.g2.hasFieldErrors === false)

    val gsm = gs._model
    assert(g === gsm)


    val fv = Map[String, Seq[String]](fs.f1._name -> fs.f1.view, fs.f2._name -> fs.f2.view, fs.f3._name -> fs.f3.view)
    val fp = F.parse(fv)
    assert(f === fp._model)

    val hs = H.fill(H.FV(5))

  }

  test("field names") {
    val f = F.FV(2, Some(4), Seq(1, 2))
    val g = G.FV(f, 4, f)
    val gs = G.fill(g)
    assert(gs.g1.f1._name === Name("G") + "g1" + "f1")
    assert(gs.g2._name === Name("G") + "g2")
  }

  test("field constraints") {
    val fs = F.fill(F.FV(5, None, Seq()))
    assert(fs.f1._errors.size == 2)
  }

  test("form constraints") {
    val fs = F.fill(F.FV(4, None, Seq()))
    assert(fs.f1._errors.size == 1)
    assert(fs.f1._errors.head == Error("odd"))
  }

  test("typesafe rendering") {
    val fs = F.fill(F.FV(4, None, Seq(2)))

    import eu.swdev.web.form.{FieldState, CState, IsSet}

    val simpleRenderer: FieldState[_, _, _] => String =
      state => s"simple renderer - state: $state"

    val enumRenderer: FieldState[_, _, CState { type EN = IsSet }] => String =
      state => s"enum renderer - state: $state; enum: ${state.constraints.en.get}"

    simpleRenderer(fs.f1)
    enumRenderer(fs.f1)

    simpleRenderer(fs.f2)
    assertTypeError("enumRenderer(fs.f2)")

  }
}
