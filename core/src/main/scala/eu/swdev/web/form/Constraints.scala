package eu.swdev.web.form

/**
  */
case class Constraints[V, M, +CS <: CState](handler: FieldHandler[V, M], lb: Option[Bound[V]], ub: Option[Bound[V]], en: Option[Enum[V]], sChecks: Seq[Check[String]], vChecks: Seq[Check[V]], mChecks: Seq[Check[M]]) {

  def le(v: V, error: Error)(implicit ev: Ordering[V]) = copy[V, M, CS { type LB = IsSet }](lb = Some(Bound(v, error, _ > 0)))
  def lt(v: V, error: Error)(implicit ev: Ordering[V]) = copy[V, M, CS { type LB = IsSet }](lb = Some(Bound(v, error, _ >= 0)))
  def ge(v: V, error: Error)(implicit ev: Ordering[V]) = copy[V, M, CS { type UB = IsSet }](ub = Some(Bound(v, error, _ < 0)))
  def gt(v: V, error: Error)(implicit ev: Ordering[V]) = copy[V, M, CS { type UB = IsSet }](ub = Some(Bound(v, error, _ <= 0)))
  def enum(v: Seq[V], error: Error) = copy[V, M, CS { type EN = IsSet }](en = Some(Enum(v, error)))

  def addSCheck(check: Check[String]) = copy[V, M, CS](sChecks = check +: sChecks)
  def addVCheck(check: Check[V]) = copy[V, M, CS](vChecks = check +: vChecks)
  def addMCheck(check: Check[M]) = copy[V, M, CS](mChecks = check +: mChecks)

  def check(model: M): Seq[Error] = {
    val mErrs = mChecks.foldLeft(Seq[Error]())((accu, check) => check(accu, model))
    handler.foldField(model)(mErrs)((accu, value) => {
      val vErrs1 = vChecks.foldLeft(accu)((errs, vCheck) => vCheck(errs, value))
      val vErrs2 = builtInVChecks.foldLeft(vErrs1)((errs, vCheck) => vCheck(errs, value))
      sChecks.foldLeft(vErrs2)((errs, sCheck) => sCheck(errs, handler.simpleConverter.format(value)))
    })
  }

  def builtInVChecks: Seq[Check[V]] = {
    (lb.collect{ case c => c } ++ ub.collect{ case c => c } ++ en.collect{ case c => c }).toList
  }
}

object Constraints {
  def apply[V, M, CS <: CState](handler: FieldHandler[V, M]): Constraints[V, M, CS] = Constraints[V, M, CS](handler, None, None, None, Nil, Nil, Nil)
}

case class Bound[V: Ordering](value: V, error: Error, chk: Int => Boolean) extends ((Seq[Error], V) => Seq[Error]) {
  def apply(errors: Seq[Error], v: V): Seq[Error] = if (chk(implicitly[Ordering[V]].compare(v, value))) {
    error +: errors
  } else {
    errors
  }
}

case class Enum[V](seq: Seq[V], error: Error) extends ((Seq[Error], V) => Seq[Error]) {
  def apply(errors: Seq[Error], v: V): Seq[Error] = if (!seq.exists(_ == v)) {
    error +: errors
  } else {
    errors
  }
}

trait Unset
trait IsSet extends Unset

/**
 * Tracks which kinds of constraints have been set in a constraint instance.
 *
 * It can be used to ensure that certain constraint kinds are set for specific operations. For example if a field
 * state is to be rendered by a drop down box then the enumeration constraint must be set in order to supply the possible
 * values.
 */
trait CState {
  type LB <: Unset
  type UB <: Unset
  type EN <: Unset
  type OC <: Occurrence
}

sealed trait Occurrence

sealed trait ZeroOrMore extends Occurrence

sealed trait AtMostOne extends Occurrence

sealed trait ZeroOrOne extends AtMostOne

sealed trait ExactlyOne extends AtMostOne
