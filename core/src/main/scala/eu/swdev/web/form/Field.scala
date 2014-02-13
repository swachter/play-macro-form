package eu.swdev.web.form

case class Field[VP, BP[_], CSP <: CState](converter: FieldConverter[VP, BP], constraints: Constraints[VP, CSP]) {

  type V = VP
  type B[X] = BP[X]
  type CS = CSP

  def le(v: VP)(implicit ev: Ordering[VP]) = Field(converter, constraints.le(v))
  def lt(v: VP)(implicit ev: Ordering[VP]) = Field(converter, constraints.lt(v))
  def ge(v: VP)(implicit ev: Ordering[VP]) = Field(converter, constraints.ge(v))
  def gt(v: VP)(implicit ev: Ordering[VP]) = Field(converter, constraints.gt(v))
  def enum(seq: Seq[VP]) = Field(converter, constraints.enum(seq))

  def doParse(name: Name, map: Map[String, Seq[String]]): FieldState[VP, BP[VP], CSP] = {
    val view = map.getOrElse(name.value, Seq())
    converter.parse(view) match {
      case Left(e) => FieldStateWithoutModel[VP, BP, CSP](name, view, constraints)
      case Right(m) => FieldStateWithModel[VP, BP, CSP](name, view, constraints, m)(converter)
    }
  }

  def doFill(name: Name, model: BP[VP]): FieldState[VP, BP[VP], CSP] = {
    val view = converter.format(model)
    FieldStateWithModel[VP, BP, CSP](name, view, constraints, model)(converter)
  }

}
