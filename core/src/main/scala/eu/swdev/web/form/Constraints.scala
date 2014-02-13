package eu.swdev.web.form

/**
  */
case class Constraints[V, +CS <: CState](lb: Option[Bound[V]], ub: Option[Bound[V]], en: Option[Seq[V]]) {

  def le(v: V)(implicit ev: Ordering[V]) = copy[V, CS { type LB = Set }](lb = Some(Bound(v, "valErr.le", _ > 0)))
  def lt(v: V)(implicit ev: Ordering[V]) = copy[V, CS { type LB = Set }](lb = Some(Bound(v, "valErr.lt", _ >= 0)))
  def ge(v: V)(implicit ev: Ordering[V]) = copy[V, CS { type UB = Set }](ub = Some(Bound(v, "valErr.ge", _ < 0)))
  def gt(v: V)(implicit ev: Ordering[V]) = copy[V, CS { type UB = Set }](ub = Some(Bound(v, "valErr.gt", _ <= 0)))
  def enum(seq: Seq[V]) = copy[V, CS { type EN = Set }](en = Some(seq))

  def check(value: V): Seq[String] = {
    var errors = Seq[String]()
    lb.foreach(bound => errors = bound.check(value, errors))
    ub.foreach(bound => errors = bound.check(value, errors))
    en.foreach(seq => if (!seq.exists(_ == value)) {
      errors = "valErr.enum" +: errors
    })
    errors
  }
}

object Constraints {
  def apply[V](): Constraints[V, CState] = Constraints[V, CState](None, None, None)
}

case class Bound[V: Ordering](value: V, error: String, chk: Int => Boolean) {
  def check(v: V, errors: Seq[String]): Seq[String] = if (chk(implicitly[Ordering[V]].compare(v, value))) {
    error +: errors
  } else {
    errors
  }
}

trait Unset
trait Set extends Unset

/**
 * The CState type tracks which kinds of constraints have been set in a constraint instance.
 *
 * It can be used to ensure that certain constraint kinds are set for specific operations. For example if a field
 * state is to be rendered by a drop down box then the enumeration constraint must be set in order to supply the possible
 * values.
 */
trait CState {
  type LB <: Unset
  type UB <: Unset
  type EN <: Unset
}

