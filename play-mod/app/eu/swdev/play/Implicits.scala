package eu.swdev.play

/**
 *
 */
trait Implicits {

  implicit val `eu.swdev.web.style.AsAttrValue[play.api.mvc.Call]` = callAsAttrValue
}
