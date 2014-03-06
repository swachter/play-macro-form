package com.abc

import eu.swdev.web.form.{FieldState, CState, IsSet, IsSetIncl, Name, Error, Form}
import org.scalatest.FunSuite

/** Tests the Form macro annotation
  */
class MacroTest extends FunSuite {

  @Form
  object F {
    val f1 = field[Int].gt(1).lt(5).enum(Seq(2,3,4))
    val f2 = field[Option[Int]]
    val f3 = field[Seq[Int]].ge(1).le(9)

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

  test("constraint type tracking") {
    val fs = F.fill(F.FV(4, None, Seq(2)))

    case class FieldTest[V, M, CS <: CState](val fs: FieldState[V, M, CS]) {
      def mustHaveEnum(implicit ev: CS <:< CState { type EN = IsSet }): Unit = {}
      def mustHaveLeGe(implicit ev: CS <:< CState { type LB = IsSetIncl; type UB = IsSetIncl }): Unit = {}
    }

    FieldTest(fs.f1).mustHaveEnum
    assertTypeError("FieldTest(fs.f2).mustHaveEnum")
    FieldTest(fs.f3).mustHaveLeGe
    assertTypeError("FieldTest(fs.f1).mustHaveLeGe")
    assertTypeError("FieldTest(fs.f2).mustHaveLeGe")

  }
}
