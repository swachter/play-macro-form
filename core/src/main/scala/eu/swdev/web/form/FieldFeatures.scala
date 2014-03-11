package eu.swdev.web.form

/**
 * Tracks the features of a field.
 *
 * This type can be used to ensure that a field has certain features that are required for some specific operations.
 * For example if a field is to be rendered by a drop down box then an enumeration constraint must exist for the field
 * that supplies the possible values.
 */
trait FieldFeatures {
  type LB <: Unset
  type UB <: Unset
  type EN <: Unset
  type OC <: Occurrence
}

trait Unset
trait IsSet extends Unset
trait IsSetIncl extends IsSet
trait IsSetExcl extends IsSet

/**
 * Tracks the occurrence of field values. For example field values can be sequences of primitive values or optional values.
 * In that case the occurrence is ZeroOrMore or ZeroOrOne, respectively.
 */
sealed trait Occurrence

sealed trait ZeroOrMore extends Occurrence

sealed trait AtMostOne extends Occurrence

sealed trait ZeroOrOne extends AtMostOne

sealed trait ExactlyOne extends AtMostOne

trait OccurrenceEvidence[O <: Occurrence] {
  def isMultiple: Boolean
}