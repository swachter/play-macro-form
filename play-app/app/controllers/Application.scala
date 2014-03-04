package controllers

import play.api.mvc._
import com.abc.F
import eu.swdev.web.play._
import eu.swdev.web.form.{FieldState, CState, Name}
import eu.swdev.web.style.Style

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

  /**
   * Create a style by applying style modifications to an empty style.
   */
  implicit val bootstrapStyle = Style(
    Bss.form += ("class", "form-horizontal"), // form-horizontal
    Bss.formGroup += ("class", "form-group"),
    Bss.label += ("class", "control-label col-xs-3"),
    Bss.inputDiv += ("class", "col-xs-9"),
    Bss.input += ("class", "form-control"),
    Bss.button += ("class", "btn") // btn-default
  )

}