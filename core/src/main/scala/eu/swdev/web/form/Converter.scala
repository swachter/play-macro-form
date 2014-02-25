package eu.swdev.web.form

/**
  */
trait SimpleConverter[V] {

  def format(t: V): String
  def parse(s: String): Either[Error, V]

}

