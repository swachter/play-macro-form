package abc

import eu.swdev.play.form._
import org.scalatest.FunSuite

/**
  */
class Test extends FunSuite {

  @Form
  object X {
    val x1 = field[Int]
    val x2 = field[Int, Option]
  }

  @Form
  object Y {
    val y1 = X
    val y2 = field[Int]
    val y3 = X
  }

  test("test") {
    val x = X.FV(5, Some(4))
    val y = Y.FV(x, 3, x)
  }
}
