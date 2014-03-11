package eu.swdev.web.form

/**
 * Describes an input field.
 *
 * @param handler
 * @param required
 * @param lb
 * @param ub
 * @param en
 * @param sChecks
 * @param vChecks
 * @param mChecks
 *
 * @tparam VP
 * @tparam MP
 * @tparam FP Tracks field features by a type.
 */
case class Field[VP, MP, +FP <: FieldFeatures](handler: FieldHandler[VP, MP], required: Option[Error], lb: Option[Bound[VP]], ub: Option[Bound[VP]], en: Option[Enum[VP]], sChecks: Seq[Check[String]], vChecks: Seq[Check[VP]], mChecks: Seq[Check[MP]]) {

  type V = VP
  type M = MP
  type F = FP

  def req(error: Error = null) = copy[V, M, F](required = Some(err(error, Error("required"))))
  def le(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = copy[V, M, F { type UB = IsSetIncl }](ub = Some(Bound(v, err(error, Error("comp.le", v)), _ > 0)))
  def lt(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = copy[V, M, F { type UB = IsSetExcl }](ub = Some(Bound(v, err(error, Error("comp.lt", v)), _ >= 0)))
  def ge(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = copy[V, M, F { type LB = IsSetIncl }](lb = Some(Bound(v, err(error, Error("comp.ge", v)), _ < 0)))
  def gt(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = copy[V, M, F { type LB = IsSetExcl }](lb = Some(Bound(v, err(error, Error("comp.gt", v)), _ <= 0)))
  def enum(v: Seq[VP], error: Error = null) = copy[V, M, F { type EN = IsSet }](en = Some(Enum(v, err(error, Error("enum", v)))))

  def addSCheck(check: Check[String]) = copy[V, M, F](sChecks = check +: sChecks)
  def addVCheck(check: Check[V]) = copy[V, M, F](vChecks = check +: vChecks)
  def addMCheck(check: Check[M]) = copy[V, M, F](mChecks = check +: mChecks)

  def parse(map: Map[String, Seq[String]], validate: Boolean, name: Name): FieldState[VP, MP, FP] = {
    val view = map.getOrElse(name.toString, map.getOrElse(name.toString + ".default", Seq()))
    handler.parse(view) match {
      case Left(e) => FieldStateWithoutModel[VP, MP, FP](name, view, this)(validate).addErrors(e)
      case Right(m) => FieldStateWithModel[VP, MP, FP](name, view, this, m)(validate)
    }
  }

  def fill(model: MP, validate: Boolean, name: Name): FieldState[VP, MP, FP] = {
    val view = handler.format(model)
    FieldStateWithModel[VP, MP, FP](name, view, this, model)(validate)
  }

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

  private def err(error: Error, defaultError: => Error): Error = if (error != null) error else defaultError
}

object Field {
  def apply[V, M, CS <: FieldFeatures](handler: FieldHandler[V, M]): Field[V, M, CS] = Field[V, M, CS](handler, None, None, None, None, Nil, Nil, Nil)
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

