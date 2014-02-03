package abc

import eu.swdev.play.form._
import org.scalatest.FunSuite

/**
  */
class Test extends FunSuite {

  @Form
  object F {
    val f1 = field[Int].lt(5).gt(1)
    val f2 = field[Int, Option]
  }

  @Form
  object G {
    val g1 = F
    val g2 = field[Int]
    val g3 = F
  }

  test("test") {
    val f = F.FV(5, Some(4))
    val g = G.FV(f, 3, f)

    val fs = F.fill(f)
    println(s"fs: $fs")
    val fsm = fs.model
    assert(f === fsm)

    val gs = G.fill(g)
    println(s"gs: $gs")
    val gsm = gs.model
    assert(g === gsm)

    val fv = Map(fs.f1.name.toString -> fs.f1.view, fs.f2.name.toString -> fs.f2.view)
    val fp = F.parse(fv)
    assert(f === fp.model)
  }
}
