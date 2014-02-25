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
    val formState = F.fill(F.FV(5, false, Seq(), 1, None))
    Ok(views.html.formTest(formState))
  }

  def processForm = Action { implicit request =>
    val formState = F.parse(request.queryString)
    Ok(views.html.formTest(formState))

  }

  implicit val bootstrapAttrs = BootstrapAttrs(
    formGroup = Attrs("class", "form-group"),
    label = Attrs("class", "col-md-4 control-label"),
    inputDiv = Attrs("class", "col-md-4"),
    input = Attrs()
  )

}