package controllers

import play.api._
import play.api.mvc._
import com.abc.F
import eu.swdev.web.play.FormSupportImpl

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def test = Action {
    val fs = F.fill(F.FV(5))
    Ok(views.html.formTest(fs)(new FormSupportImpl))
  }

}