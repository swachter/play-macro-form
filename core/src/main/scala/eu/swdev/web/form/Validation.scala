package eu.swdev.web.form

/**
  */
trait Validation {
  def validate(parseErrors: Seq[Error], validationErrors: => Seq[Error]): Seq[Error]
}

object WithValidation extends Validation {
  def validate(parseErrors: Seq[Error], validationErrors: => Seq[Error]) = validationErrors ++ parseErrors
}

object WithoutValidation extends Validation {
  def validate(parseErrors: Seq[Error], validationErrors: => Seq[Error]) = Nil
}