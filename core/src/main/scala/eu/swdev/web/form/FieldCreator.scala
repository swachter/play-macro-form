package eu.swdev.web.form

/**
 * Provides a method to create a field. Field creators are used as an implicit parameter for the field[M] method.
 * The compiler searches for a fitting FieldCreator. The found FieldCreator fixes the types V and CS that represent
 * certain features of the field.
 *
 * @tparam M The type parameter is
 */
trait FieldCreator[M] {
  type V
  type CS <: FieldFeatures
  def createField: Field[V, M, CS]
}
