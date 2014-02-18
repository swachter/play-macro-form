package eu.swdev.web.form

case class Field[VP, MP, CSP <: CState](constraints: Constraints[VP, MP, CSP]) {

  type V = VP
  type M = MP
  type CS = CSP

  def le(v: VP)(implicit ev: Ordering[VP]) = Field(constraints.le(v))
  def lt(v: VP)(implicit ev: Ordering[VP]) = Field(constraints.lt(v))
  def ge(v: VP)(implicit ev: Ordering[VP]) = Field(constraints.ge(v))
  def gt(v: VP)(implicit ev: Ordering[VP]) = Field(constraints.gt(v))
  def enum(seq: Seq[VP]) = Field(constraints.enum(seq))

  def addSCheck(check: Check[String]) = Field(constraints.addSCheck(check))
  def addVCheck(check: Check[V]) = Field(constraints.addVCheck(check))
  def addMCheck(check: Check[M]) = Field(constraints.addMCheck(check))


  def doParse(name: Name, map: Map[String, Seq[String]]): FieldState[VP, MP, CSP] = {
    val view = map.getOrElse(name.value, map.getOrElse(name.value + ".default", Seq()))
    constraints.handler.parse(view) match {
      case Left(e) => FieldStateWithoutModel[VP, MP, CSP](name, view, constraints).addErrors(e)
      case Right(m) => FieldStateWithModel[VP, MP, CSP](name, view, constraints, m)
    }
  }

  def doFill(name: Name, model: MP): FieldState[VP, MP, CSP] = {
    val view = constraints.handler.format(model)
    FieldStateWithModel[VP, MP, CSP](name, view, constraints, model)
  }

}
