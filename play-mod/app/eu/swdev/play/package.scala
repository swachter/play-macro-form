package eu.swdev

import _root_.play.api.mvc.Call
import eu.swdev.web.style.AsAttrValue

/**
  */
package object play {

  /**
   * Typeclass instance that allows to use Call objects as attribute values.
   */
  implicit val callAsAttrValue = new AsAttrValue[Call] {
    override def asStringSet(value: Call): Set[String] = Set(value.toString())
    override def asString(value: Call): String = value.toString()
  }


}
