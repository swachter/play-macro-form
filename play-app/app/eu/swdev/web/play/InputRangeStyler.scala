package eu.swdev.web.play

import eu.swdev.web.style._
import eu.swdev.web.form.SimpleConverter

/**
 * Provides a function that adds the necessary Html attributes for an input element with type="range".
 *
 * @tparam V
 */
trait InputRangeStyler[V] {
  def apply(lb: V, ub: V, sc: SimpleConverter[V]): AttrsT = Attrs ~= (type_@, "range") ~= (min_@, sc.format(lb)) ~= (max_@, sc.format(ub)) ~= step(lb, ub, sc)
  protected def step(lb: V, ub: V, sc: SimpleConverter[V]): Option[Attr]
}

/**
  */
object InputRangeStyler {
  implicit val IntInputRangeStyler = new InputRangeStyler[Int] {
    override def step(lb: Int, ub: Int, sc: SimpleConverter[Int]): Option[Attr] = None
  }
  implicit val DoubleInputRangeStyler = new InputRangeStyler[Double] {
    override def step(lb: Double, ub: Double, sc: SimpleConverter[Double]): Option[Attr] = Some(Attr(step_@, sc.format(((ub - lb) / 1000))))
  }
}
