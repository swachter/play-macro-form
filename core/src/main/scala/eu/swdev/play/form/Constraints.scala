package eu.swdev.play.form

/**
  */
case class Constraints[M, +LB <: Option[LowerBound[M]], +UB <: Option[UpperBound[M]], +EN <: Option[Enum[M]]](lb: LB, ub: UB, en: EN) {
  def le(m: M) = copy[M, Some[LowerBound[M]], UB, EN](lb = Some(Le(m)))
  def lt(m: M) = copy[M, Some[LowerBound[M]], UB, EN](lb = Some(Lt(m)))
  def ge(m: M) = copy[M, LB, Some[UpperBound[M]], EN](ub = Some(Ge(m)))
  def gt(m: M) = copy[M, LB, Some[UpperBound[M]], EN](ub = Some(Gt(m)))
  def enum(seq: Seq[M]) = copy[M, LB, UB, Some[Enum[M]]](en = Some(Enum(seq)))
}

sealed trait LowerBound[M]
case class Le[M](value: M) extends LowerBound[M]
case class Lt[M](value: M) extends LowerBound[M]

sealed trait UpperBound[M]
case class Ge[M](value: M) extends UpperBound[M]
case class Gt[M](value: M) extends UpperBound[M]

case class Enum[M](value: Seq[M])