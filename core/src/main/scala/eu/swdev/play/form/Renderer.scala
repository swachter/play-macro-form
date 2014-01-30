package eu.swdev.play.form

/**
  */
trait Renderer[M, C[_], -LB <: Option[LowerBound[M]], -UB <: Option[UpperBound[M]], -EN <: Option[Enum[M]]] {
  def render(fieldName: String, fieldState: FieldState[C[M]], constraints: Constraints[M, LB, UB, EN]): String
}
