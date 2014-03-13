package eu.swdev.web.form

/**
  */
trait SimpleConverter[V] {

  type BiV <: Bool // tracks if V has exactly two different values (i.e. it is bi-valued)

  def format(t: V): String
  def parse(s: String): Either[Error, V]

  def createSimpleFieldHandler: FieldHandler[V, V]
}

trait SimpleConverterWithoutBiStateSupport[V] extends SimpleConverter[V] {
  type BiV = False
  def createSimpleFieldHandler = new SimpleFieldHandlerWithoutBiStateSupport(this)
}

trait SimpleConverterWithBiStateSupport[V] extends SimpleConverter[V] {
  type BiV = True
  def createSimpleFieldHandler = new SimpleFieldHandlerWithBiStateSupport(this, checkedValue, uncheckedValue)
  def checkedValue: V
  def uncheckedValue: V
}