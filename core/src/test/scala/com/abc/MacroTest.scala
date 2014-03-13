package com.abc

import eu.swdev.web.form._
import org.scalatest.FunSuite
import eu.swdev.web.form.Error
import scala.Some

/** Tests the Form macro annotation
  */
class MacroTest extends FunSuite {

  @Form
  object F {
    val f1 = field[Int].gt(1).lt(5).enum(Seq(2,3,4))
    val f2 = field[Option[Int]].addVCheck(v => if (v % 2 == 0) Some(Error("odd")) else None)
    val f3 = field[Seq[Int]].ge(1).le(9)
  }

  @Form
  object G {
    val g1 = F
    val g2 = field[Int]
    val g3 = F

    def validate(fs: FS): Option[Error] = {
      if (fs.g1.f1._model != fs.g3.f1._model) Some(Error("notEqual")) else None
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
    assert(f === fs._model)

    val gs = G.fill(g)
    assert(g === gs._model)

    val fv = Map[String, Seq[String]](fs.f1._name -> fs.f1.view, fs.f2._name -> fs.f2.view, fs.f3._name -> fs.f3.view)
    val fp = F.parse(fv)
    assert(f === fp._model)

    val hs = H.fill(H.FV(5))

  }

  test("field names") {
    val gs = G.parse(Map())
    assert(gs.g1.f1._name === Name("G") + "g1" + "f1")
    assert(gs.g2._name === Name("G") + "g2")
    val gs2 = G.parse(Map(), name = Name.empty)
    assert(gs2.g1.f1._name === Name("g1") + "f1")
    assert(gs2.g2._name === Name("g2"))
  }

  test("field constraints") {
    val fs = F.fill(F.FV(3, Some(1), Seq(1)))
    assert(fs.f1._errors.size == 0)
    assert(fs.f2._errors.size == 0)
    assert(fs.f3._errors.size == 0)
    val fs2 = F.fill(F.FV(5, Some(2), Seq(10)))
    assert(fs2.f1._errors.size == 2)
    assert(fs2.f2._errors.size == 1)
    assert(fs2.f2._errors.head == Error("odd"))
    assert(fs2.f3._errors.size == 1)
  }

  test("form constraints") {
    val fv1 = F.FV(4, None, Seq())
    val fv2 = F.FV(3, None, Seq())
    val gs1 = G.fill(G.FV(fv1, 0, fv1))
    assert(gs1._errors.size == 0)
    val gs2 = G.fill(G.FV(fv1, 0, fv2))
    assert(gs2._errors.size == 1)
    assert(gs2._errors.head == Error("notEqual"))
  }

  @Form
  object CheckBoxForm {
    val f1 = field[Boolean]
    val f2 = field[Option[Boolean]]
    val f3 = field[Seq[Boolean]]
    val f4 = field[Int]
  }

  test("constraint type tracking") {
    val fs = F.fill(F.FV(4, None, Seq(2)))

    case class FieldTest[V, M, CS <: FieldFeatures](val fs: FieldState[V, M, CS]) {
      def mustHaveEnum(implicit ev: CS <:< FieldFeatures { type EN = IsSet }): Unit = {}
      def mustHaveLeGe(implicit ev: CS <:< FieldFeatures { type LB = IsSetIncl; type UB = IsSetIncl }): Unit = {}
      def mustBeBiValued(implicit ev: CS <:< FieldFeatures { type BiV = True; type OC = ExactlyOne }, ev2: V =:= M): Unit = {}
    }

    FieldTest(fs.f1).mustHaveEnum
    assertTypeError("FieldTest(fs.f2).mustHaveEnum")
    FieldTest(fs.f3).mustHaveLeGe
    assertTypeError("FieldTest(fs.f1).mustHaveLeGe")
    assertTypeError("FieldTest(fs.f2).mustHaveLeGe")

    val cbf = CheckBoxForm.parse(Map())

    FieldTest(cbf.f1).mustBeBiValued
    assertTypeError("FieldTest(cbf.f2).mustBeBiValued")
    assertTypeError("FieldTest(cbf.f3).mustBeBiValued")
    assertTypeError("FieldTest(cbf.f4).mustBeBiValued")

  }

  test("parse without validation") {
    val fs = F.parse(Map(), WithoutValidation)
    assert(fs.hasFieldErrors === false)
    assert(fs.hasFormErrors === false)
  }
}
