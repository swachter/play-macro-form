package eu.swdev.play.form

/**
  */
class FormFactory[F <: BaseForm](val apply: () => F)