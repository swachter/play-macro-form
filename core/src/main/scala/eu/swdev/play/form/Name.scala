package eu.swdev.play.form

/**
  */
class Name(val name: String) extends AnyVal {

  def +(s: String): Name = if (name.isEmpty) new Name(s) else new Name(s"$name.$s")

  override def toString: String = name
}
