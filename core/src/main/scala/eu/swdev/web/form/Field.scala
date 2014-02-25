package eu.swdev.web.form

case class Field[VP, MP, CSP <: CState](constraints: Constraints[VP, MP, CSP]) {

  type V = VP
  type M = MP
  type CS = CSP

  def le(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = Field(constraints.le(v, err(error, Error("comp.le", v))))
  def lt(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = Field(constraints.lt(v, err(error, Error("comp.lt", v))))
  def ge(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = Field(constraints.ge(v, err(error, Error("comp.ge", v))))
  def gt(v: VP, error: Error = null)(implicit ev: Ordering[VP]) = Field(constraints.gt(v, err(error, Error("comp.gt", v))))
  def enum(v: Seq[VP], error: Error = null) = Field(constraints.enum(v, err(error, Error("enum", v))))

  def addSCheck(check: Check[String]) = Field(constraints.addSCheck(check))
  def addVCheck(check: Check[V]) = Field(constraints.addVCheck(check))
  def addMCheck(check: Check[M]) = Field(constraints.addMCheck(check))

  def doParse(name: Name, map: Map[String, Seq[String]]): FieldState[VP, MP, CSP] = {
    val view = map.getOrElse(name.toString, map.getOrElse(name.toString + ".default", Seq()))
    constraints.handler.parse(view) match {
      case Left(e) => FieldStateWithoutModel[VP, MP, CSP](name, view, constraints).addErrors(e)
      case Right(m) => FieldStateWithModel[VP, MP, CSP](name, view, constraints, m)
    }
  }

  def doFill(name: Name, model: MP): FieldState[VP, MP, CSP] = {
    val view = constraints.handler.format(model)
    FieldStateWithModel[VP, MP, CSP](name, view, constraints, model)
  }

  private def err(error: Error, defaultError: => Error): Error = if (error != null) error else defaultError
}
