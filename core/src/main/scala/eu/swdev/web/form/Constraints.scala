package eu.swdev.web.form

/**
  */
case class Constraints[V, M, +CS <: CState](handler: FieldHandler[V, M], lb: Option[Bound[V]], ub: Option[Bound[V]], en: Option[Enum[V]], sChecks: Seq[Check[String]], vChecks: Seq[Check[V]], mChecks: Seq[Check[M]]) {

  def le(v: V, errCode: String = "valErr.le")(implicit ev: Ordering[V]) = copy[V, M, CS { type LB = Set }](lb = Some(Bound(v, errCode, _ > 0)))
  def lt(v: V, errCode: String = "valErr.lt")(implicit ev: Ordering[V]) = copy[V, M, CS { type LB = Set }](lb = Some(Bound(v, errCode, _ >= 0)))
  def ge(v: V, errCode: String = "valErr.ge")(implicit ev: Ordering[V]) = copy[V, M, CS { type UB = Set }](ub = Some(Bound(v, errCode, _ < 0)))
  def gt(v: V, errCode: String = "valErr.gt")(implicit ev: Ordering[V]) = copy[V, M, CS { type UB = Set }](ub = Some(Bound(v, errCode, _ <= 0)))
  def enum(seq: Seq[V], errCode: String = "valErr.enum") = copy[V, M, CS { type EN = Set }](en = Some(Enum(seq, errCode)))

  def addSCheck(check: Check[String]) = copy[V, M, CS](sChecks = check +: sChecks)
  def addVCheck(check: Check[V]) = copy[V, M, CS](vChecks = check +: vChecks)
  def addMCheck(check: Check[M]) = copy[V, M, CS](mChecks = check +: mChecks)

  def check(model: M): Seq[String] = {
    val mErrs = mChecks.foldLeft(Seq[String]())((accu, check) => check(accu, model))
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
  def apply[V, M](handler: FieldHandler[V, M]): Constraints[V, M, CState] = Constraints[V, M, CState](handler, None, None, None, Nil, Nil, Nil)
}

case class Bound[V: Ordering](value: V, error: String, chk: Int => Boolean) extends ((Seq[String], V) => Seq[String]) {
  def apply(errors: Seq[String], v: V): Seq[String] = if (chk(implicitly[Ordering[V]].compare(v, value))) {
    error +: errors
  } else {
    errors
  }
}

case class Enum[V](seq: Seq[V], error: String) extends ((Seq[String], V) => Seq[String]) {
  def apply(errors: Seq[String], v: V): Seq[String] = if (!seq.exists(_ == v)) {
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

