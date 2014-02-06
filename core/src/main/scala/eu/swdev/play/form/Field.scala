package eu.swdev.play.form

case class Field[V, B[_], CS <: CState](converter: FieldConverter[V, B], constraints: Constraints[V, CS]) {

  def le(v: V)(implicit ev: Ordering[V]) = Field(converter, constraints.le(v))
  def lt(v: V)(implicit ev: Ordering[V]) = Field(converter, constraints.lt(v))
  def ge(v: V)(implicit ev: Ordering[V]) = Field(converter, constraints.ge(v))
  def gt(v: V)(implicit ev: Ordering[V]) = Field(converter, constraints.gt(v))
  def enum(seq: Seq[V]) = Field(converter, constraints.enum(seq))

  def doParse(name: Name, map: Map[String, Seq[String]]): FieldState[B[V], Constraints[V, CS]] = {
    val view = map.getOrElse(name.value, Seq())
    converter.parse(view) match {
      case Left(e) => FieldStateWithoutModel[V, B, Constraints[V, CS]](name, view, constraints)
      case Right(m) => FieldStateWithModel(name, view, constraints, m)(converter)
    }
  }

  def doFill(name: Name, model: B[V]): FieldState[B[V], Constraints[V, CS]] = {
    val view = converter.format(model)
    FieldStateWithModel(name, view, constraints, model)(converter)
  }

}
