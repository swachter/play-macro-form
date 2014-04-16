import com.abc.F
import eu.swdev.web.style.Style
import org.scalatest.FunSuite

import eu.swdev.play.form.bootstrap3._
import play.api.i18n.Lang

import play.api.test.FakeApplication
import play.api.test.Helpers
/**
  */
class RendererTest extends FunSuite {

  test("type safe rendering") {

    Helpers.running(FakeApplication(additionalPlugins = Seq("eu.swdev.play.i18n.TestFormResourceApiImpl"))) {
      // Ensure that certain ways to input data can only be rendered if the field is suitable for that way of input
      // e.g. a checkBoxGroup requires that an enumeration of possible values was specified.

      implicit val style = Style()
      implicit val lang = Lang("de", "DE")
      val fs = F.parse(Map.empty)
      fs.f1.inputRange
      fs.f3.selectionGroup(true)
      fs.f4.selectionList()

      assertTypeError("fs.f2.inputRange")
      assertTypeError("fs.f6.selectionGroup(true)")
      assertTypeError("fs.f8.selectionList()")


    }

  }

}
