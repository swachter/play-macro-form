package abc

import eu.swdev.play.form._
import org.scalatest.FunSuite

/**
  */
class Test extends FunSuite {

  @Form
  object F {
    val f1 = field[Int]
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

    val fs = F.fill(new Name(""), f)
    println(s"fs: $fs")
    val gs = G.fill(new Name(""), g)
    println(s"gs: $gs")
  }
}
