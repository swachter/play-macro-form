package eu.swdev.web.form

/**
  */
trait SimpleConverter[V] {

  /**
   * Indicates if the type V is bi-valued, i.e. that it can have exactly two different values.
   *
   * If a type is bi-valued then it can be input by a single check box.
   */
  type BiV <: Bool

  def format(t: V): String
  def parse(s: String): Either[Error, V]

  def createSimpleFieldHandler: FieldHandler[V, V]
}

trait SimpleConverterWithoutBiStateSupport[V] extends SimpleConverter[V] {
  type BiV = False
  def createSimpleFieldHandler = new SimpleFieldHandlerWithoutBiStateSupport(this)
}

/**
 * A simple converter for a bi-valued type.
 *
 * The simple converter must declare which value is used for the "checked" state and which for the "unchecked" state.
 * @tparam V
 */
trait SimpleConverterWithBiStateSupport[V] extends SimpleConverter[V] {
  type BiV = True
  def createSimpleFieldHandler = new SimpleFieldHandlerWithBiStateSupport(this, checkedValue, uncheckedValue)
  def checkedValue: V
  def uncheckedValue: V
}