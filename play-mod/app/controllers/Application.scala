package controllers

import play.api.mvc._
import com.abc.F
import eu.swdev.web.form._
import eu.swdev.web.style.Style
import eu.swdev.play.form.bootstrap3._
import scala.language.existentials

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def test = Action { implicit request =>
    val formState = F.parse(Map.empty, WithoutValidation)
    Ok(views.html.formTest(formState, false))
  }

  def processForm = Action { implicit request =>
    val formState = F.bindFromRequest
    formState.fold(fs => Ok(views.html.formTest(formState, true)), fv => Ok(views.html.formTest(F.fill(fv), false)))
  }

  /**
   * Create a style by applying style modifications to an empty style.
   */
  implicit val bootstrapStyle = Style(
    Bs.form += (class_@, "form-horizontal"), // form-horizontal
    Bs.formGroup += (class_@, "form-group"),
    Bs.label += (class_@, "control-label col-xs-3"),
    Bs.inputDiv += (class_@, "col-xs-9"),
    Bs.input += (class_@, "form-control"),
    Bs.button += (class_@, "btn") // btn-default
  )

  implicit class RichForm[BF <: BaseForm](val baseForm: BF) extends AnyVal {
    def bindFromRequest(implicit request: Request[_]): baseForm.FS = baseForm.parse(request.queryString)
  }
}