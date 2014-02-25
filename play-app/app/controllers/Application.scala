package controllers

import play.api.mvc._
import com.abc.F
import eu.swdev.web.play._
import eu.swdev.web.form.{FieldState, CState, Name}
import eu.swdev.web.play.BootstrapAttrs
import play.api.i18n.Lang

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def test = Action { implicit request =>
    println(s"lang2: ${implicitly[Lang]}")
    println(s"req: $request")
    println(s"req.accept: ${request.acceptLanguages}")
    val formState = F.fill(F.FV(5, false, Seq(), 1, None))
    formState.f1.addError("Fehler!")
    Ok(views.html.formTest(formState))
  }

  def processForm = Action { implicit request =>

    val formState = F.parse(request.queryString)

    println(s"lang2: ${implicitly[Lang]}")
    println(s"req: $request")
    println(formState)
    println(s"errors: ${formState.f1.errors}")
    Ok(views.html.formTest(formState))

  }

  implicit val bootstrapAttrs = BootstrapAttrs(
    formGroup = Attrs("class", "form-group"),
    label = Attrs("class", "col-md-4 control-label"),
    inputDiv = Attrs("class", "col-md-4"),
    input = Attrs()
  )

}