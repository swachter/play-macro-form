package eu.swdev.web.form

/**
 * Tracks which kinds of constraints are defined for a field.
 *
 * This type can be used to ensure that certain constraint kinds are set for specific operations. For example if a field
 * state is to be rendered by a drop down box then the enumeration constraint must be set in order to supply the possible
 * values.
 */
trait CState {
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
 * Tracks the occurrence of field values. For example field values can be sequences of values or optianal values.
 * In that case the occurrence is ZeroOrMore or ZeroOrOne, respectively.
 */
sealed trait Occurrence

sealed trait ZeroOrMore extends Occurrence

sealed trait AtMostOne extends Occurrence

sealed trait ZeroOrOne extends AtMostOne

sealed trait ExactlyOne extends AtMostOne
