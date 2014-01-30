package eu.swdev.play.form

/**
  */
case class Constraints[V, CState](lb: Option[LowerBound[V]], ub: Option[UpperBound[V]], en: Option[Seq[V]]) {
  def le(v: V) = copy[V, CState { type LB = Set }](lb = Some(Le(v)))
  def lt(v: V) = copy[V, CState { type LB = Set }](lb = Some(Lt(v)))
  def ge(v: V) = copy[V, CState { type UB = Set }](ub = Some(Ge(v)))
  def gt(v: V) = copy[V, CState { type UB = Set }](ub = Some(Gt(v)))
  def enum(seq: Seq[V]) = copy[V, CState { type EN = Set }](en = Some(seq))
}

object Constraints {
  def apply[V](): Constraints[V, CState] = Constraints[V, CState](None, None, None)
}

sealed trait LowerBound[M]
case class Le[M](value: M) extends LowerBound[M]
case class Lt[M](value: M) extends LowerBound[M]

sealed trait UpperBound[M]
case class Ge[M](value: M) extends UpperBound[M]
case class Gt[M](value: M) extends UpperBound[M]

trait Unset
trait Set extends Unset

trait CState {
  type LB <: Unset
  type UB <: Unset
  type EN <: Unset
}

