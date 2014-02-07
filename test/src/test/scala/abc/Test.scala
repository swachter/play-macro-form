package abc

import eu.swdev.play.form._
import org.scalatest.FunSuite

import eu.swdev.play.form.Name.usedAsStringKey

/**
  */
class Test extends FunSuite {

  @Form
  object F {
    val f1 = field[Int].lt(5).enum(Seq(2,3,4))
    val f2 = field[Int, Option]
    val f3 = field2[Seq[Int]]

    // method is called when a FormState is constructed
    def test(fs: FS): Unit = {
      if (fs.f1.model % 2 == 0) {
        fs.f1.addError("valErr.odd")
      }
    }
  }

  @Form
  object G {
    val g1 = F
    val g2 = field[Int]
    val g3 = F

    def validate(fs: FS): Unit = {
      if (fs.g2.model % 2 == 1) {
        fs.g2.addError("valErr.even")
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
    val fsm = fs.model
    assert(f === fsm)

    val gs = G.fill(g)
    assert(gs.g2.hasFieldErrors === false)

    val gsm = gs.model
    assert(g === gsm)

    val fv = Map[String, Seq[String]](fs.f1.name -> fs.f1.view, fs.f2.name -> fs.f2.view, fs.f3.name -> fs.f3.view)
    val fp = F.parse(fv)
    assert(f === fp.model)

    val hs = H.fill(H.FV(5))

  }

  test("field constraints") {
    val fs = F.fill(F.FV(5, None, Seq()))
    assert(fs.f1.errors.size == 2)
  }

  test("form constraints") {
    val fs = F.fill(F.FV(4, None, Seq()))
    assert(fs.f1.errors.size == 1)
    assert(fs.f1.errors.head == "valErr.odd")
  }

  test("typesafe rendering") {
    val fs = F.fill(F.FV(4, None, Seq(2)))

    def simpleRenderer[B[_]]: FieldState[_, B, _] => String =
      state => s"simple renderer - state: $state"

    def enumRenderer[B[_]]: FieldState[_, B, CState { type EN = Set }] => String =
      state => s"enum renderer - state: $state; enum: ${state.constraints.en.get}"

    simpleRenderer(fs.f1)
    enumRenderer(fs.f1)

    simpleRenderer(fs.f2)
    assertTypeError("enumRenderer(fs.f2)")

  }
}
