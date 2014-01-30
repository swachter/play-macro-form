package eu.swdev.play.form

/**
 */
case class FieldState[CM](
  var view: Seq[String]       = Seq(),
  var errors: Seq[String]     = Seq(),
  var model: Option[CM]       = None
)

