package eu.swdev.play.form

// The Field class must not be a case class because it is used as a hash key
class Field[M, C[_], LB <: Option[LowerBound[M]], UB <: Option[UpperBound[M]], EN <: Option[Enum[M]]](handler: FieldHandler[M, C], constraints: Constraints[M, LB, UB, EN]) extends Mapping {

  type Value = C[M]

  def render(renderer: Renderer[M, C, LB, UB, EN])(implicit formState: FormState[_]): String = {
    val fieldName = formState.form.asInstanceOf[BaseForm].fieldName(this)
    renderer.render(fieldName, myFieldState, constraints)
  }

  def le(m: M) = new Field(handler, constraints.le(m))
  def lt(m: M) = new Field(handler, constraints.lt(m))
  def ge(m: M) = new Field(handler, constraints.ge(m))
  def gt(m: M) = new Field(handler, constraints.gt(m))
  def enum(seq: Seq[M]) = new Field(handler, constraints.enum(seq))

  private def myFieldState(implicit formState: FormState[_]) = formState(this)

  def fill(map: Map[String, Seq[String]])(implicit formState: FormState[_]): Unit = {
    val fieldName = formState.fieldName(this)
    val view = map.getOrElse(fieldName, Seq())
    handler.fillFromView(view, myFieldState)
  }

  def set(model: C[M])(implicit formState: FormState[_]): Unit = {
    handler.fillFromModel(model, myFieldState)
  }

  def get(implicit formState: FormState[_]): C[M] = {
    myFieldState.model.get
  }
}

case class Field2[V, B[_], LB <: Option[LowerBound[V]], UB <: Option[UpperBound[V]], EN <: Option[Enum[V]]](converter: FieldConverter[B[V]], constraints: Constraints[V, LB, UB, EN]) {

  def le(v: V) = new Field2(converter, constraints.le(v))
  def lt(v: V) = new Field2(converter, constraints.lt(v))
  def ge(v: V) = new Field2(converter, constraints.ge(v))
  def gt(v: V) = new Field2(converter, constraints.gt(v))
  def enum(seq: Seq[V]) = new Field2(converter, constraints.enum(seq))

  def parse(name: Name, map: Map[String, Seq[String]]): FieldState2[B[V], Constraints[V, LB, UB, EN]] = {
    val view = map.getOrElse(name.name, Seq())
    converter.parse(view) match {
      case Left(e) => FieldStateWithoutModel[V, B, Constraints[V, LB, UB, EN]](name, view, constraints)
      case Right(m) => FieldStateWithModel(name, view, constraints, m)
    }
  }

  def fill(name: Name, model: B[V]): FieldState2[B[V], Constraints[V, LB, UB, EN]] = {
    val view = converter.format(model)
    FieldStateWithModel(name, view, constraints, model)
  }

}
