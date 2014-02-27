package controllers

import play.api.mvc._
import com.abc.F
import eu.swdev.web.play._
import eu.swdev.web.form.{FieldState, CState, Name}

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

//  implicit val bootstrapAttrs = BootstrapAttrs(
//    form = Attrs(), // form-horizontal
//    formGroup = Attrs("class", "form-group"),
//    label = Attrs("class", "control-label"), // col-md-4
//    inputDiv = Attrs(), // col-md-4
//    input = Attrs()
//  )

  implicit val bootstrapStyle = Style(
    Bss.form := Attrs("class", "form-horizontal"), // form-horizontal
    Bss.formGroup := Attrs("class", "form-group"),
    Bss.label := Attrs("class", "control-label col-xs-3"), // col-md-4
    Bss.inputDiv := Attrs("class", "col-xs-6"), // col-md-4
    Bss.input := Attrs("class", "form-control input-xs"),
    Bss.button := Attrs("class", "btn") // btn-default
  )

}