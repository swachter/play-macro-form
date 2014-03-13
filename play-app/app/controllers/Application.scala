package controllers

import play.api.mvc._
import com.abc.F
import eu.swdev.web.play._
import eu.swdev.web.form._
import eu.swdev.web.style.Style
import eu.swdev.web.form.Error

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def test = Action { implicit request =>
    val formState = F.parse(Map.empty, WithoutValidation)
    Ok(views.html.formTest(formState))
  }

  def processForm = Action { implicit request =>
    val formState = F.parse(request.queryString)
    Ok(views.html.formTest(formState))

  }

  /**
   * Create a style by applying style modifications to an empty style.
   */
  implicit val bootstrapStyle = Style(
    Bss.form += (class_@, "form-horizontal"), // form-horizontal
    Bss.formGroup += (class_@, "form-group"),
    Bss.label += (class_@, "control-label col-xs-3"),
    Bss.inputDiv += (class_@, "col-xs-9"),
    Bss.input += (class_@, "form-control"),
    Bss.button += (class_@, "btn") // btn-default
  )

}