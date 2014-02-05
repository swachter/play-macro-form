package abc

import eu.swdev.play.form._
import org.scalatest.FunSuite

/**
  */
class Test extends FunSuite {

  @Form
  object F {
    val f1 = field[Int].lt(5).enum(Seq(2,3,4))
    val f2 = field[Int, Option]
    val f3 = field2[Seq[Int]]

    def test(fs: WFS): Unit = {
      if (fs.f1.model % 2 == 0) {
        fs.f1.addError("number must be odd")
      }
    }
  }

  @Form
  object G {
    val g1 = F
    val g2 = field[Int]
    val g3 = F

    def validate(fs: WFS): Unit = {
      if (fs.g2.model % 2 == 1) {
        fs.g2.addError("number must be even")
      }
    }
  }

  test("test") {
    val f = F.FV(2, Some(4), Seq(1, 2))
    val g = G.FV(f, 4, f)

    val fs = F.fill(f)
    println(s"fs: $fs")
    assert(fs.f1.hasFieldErrors === true)
    val fsm = fs.model
    assert(f === fsm)

    val gs = G.fill(g)
    println(s"gs: $gs")
    assert(gs.g2.hasFieldErrors === false)

    val gsm = gs.model
    assert(g === gsm)

    val fv = Map(fs.f1.name.toString -> fs.f1.view, fs.f2.name.toString -> fs.f2.view, fs.f3.name.toString -> fs.f3.view)
    val fp = F.parse(fv)
    assert(f === fp.model)

    val simpleRenderer: FieldState[_, _] => String =
        state => s"simple renderer - state: $state"

    val enumRenderer: FieldState[_, Constraints[_, CState { type EN = Set }]] => String =
        state => s"enum renderer - state: $state; enum: ${state.constraints.en.get}"

    val f1sr = simpleRenderer(fs.f1)
    val f1er = enumRenderer(fs.f1)

    val g2sr = simpleRenderer(gs.g2)

    assertTypeError("val g2er = enumRenderer(gs.g2)")
  }
}