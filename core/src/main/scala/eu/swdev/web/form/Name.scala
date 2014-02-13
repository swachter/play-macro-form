package eu.swdev.web.form

import scala.language.implicitConversions

/**
  */
class Name(val value: String) extends AnyVal {

  def +(that: Name): Name = {
    if (that.value.isEmpty) {
      this
    } else if (value.isEmpty) {
      that
    } else {
      new Name(s"$value.${that.value}")
    }
  }

}

object Name {
  val empty = new Name("")
  def apply(string: String) = {
    if (string.isEmpty) empty else new Name(string)
  }
  implicit def asString(name: Name) = name.value
  implicit def asName(string: String) = apply(string)

  implicit def usedAsStringKey[A](t: (Name, A)): (String, A) = (asString(t._1), t._2)
}
