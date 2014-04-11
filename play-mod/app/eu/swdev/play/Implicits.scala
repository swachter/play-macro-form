package eu.swdev.play

import eu.swdev.play.i18n.HtmlMarkup
import play.i18n.Lang
import java.util.Locale

/**
 *
 */
trait Implicits {

  implicit val `eu.swdev.web.style.AsAttrValue[play.api.mvc.Call]` = callAsAttrValue

  implicit val `eu.swdev.play.i18n.HtmlMarkup` = HtmlMarkup

  implicit def `play.api.i18n.Lang->java.util.Locale`(implicit lang: Lang): Locale = lang.toLocale
}
