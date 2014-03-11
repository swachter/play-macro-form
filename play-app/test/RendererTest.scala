import com.abc.F
import eu.swdev.web.style.Style
import org.scalatest.FunSuite

import eu.swdev.web.play._
import play.api.i18n.Lang

/**
  */
class RendererTest extends FunSuite {

  test("type safe rendering") {

    // Ensure that certain ways to input data can only be rendered if the field is suitable for that way of input
    // e.g. a checkBoxGroup requires that an enumeration of possible values was specified.

    implicit val style = Style()
    implicit val lang = Lang
    val fs = F.parse(Map.empty, false)
    fs.f1.inputRange
    fs.f3.selectionGroup(true)
    fs.f4.selectionList()

    assertTypeError("fs.f2.inputRange")
    assertTypeError("fs.f6.selectionGroup(true)")
    assertTypeError("fs.f8.selectionList()")

  }

}
