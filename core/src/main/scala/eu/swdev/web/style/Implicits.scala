package eu.swdev.web.style

import scala.language.implicitConversions
import eu.swdev.web.EntryPoint

/**
 */
trait Implicits {

  implicit def `eu.swdev.web.style.Style->eu.swdev.web.style.RichStyle`(style: Style) = new RichStyle(style)

  implicit def `eu.swdev.web.style.Attrs->eu.swdev.web.style.RichAttrs`(attrs: Attrs) = new RichAttrs(attrs)

  implicit def `eu.swdev.web.style.AttrsT->eu.swdev.web.style.RichAttrsT`(attrsT: AttrsT) = new RichAttrsT(attrsT)

  implicit def `eu.swdev.web.style.Attrs->String`(attrs: Attrs) = attrsToString(attrs)

  implicit def `eu.swdev.web.EntryPoint->eu.swdev.web.style.AttrDescs`(entryPoint: EntryPoint.type) = AttrDescs
}

