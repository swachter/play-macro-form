package abc

import eu.swdev.play.form.Form
import org.scalatest.FunSuite

/**
  */
class Test extends FunSuite {

  @Form
  object X {
    val a = 1
  }
}
