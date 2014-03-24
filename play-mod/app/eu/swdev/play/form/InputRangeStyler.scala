package eu.swdev.play.form

import eu.swdev.web.form._
import eu.swdev.web.style._

/**
  */
object InputRangeStyler {
  implicit val IntInputRangeStyler = new InputRangeStyler[Int] {
    override def step(lb: Int, ub: Int, sc: SimpleConverter[Int]): Option[Attr] = None
  }
  implicit val DoubleInputRangeStyler = new InputRangeStyler[Double] {
    override def step(lb: Double, ub: Double, sc: SimpleConverter[Double]): Option[Attr] = Some(Attr(AttrDescs.step_@, sc.format(((ub - lb) / 1000))))
  }
}

/**
 * Provides a function that adds the necessary Html attributes for an input element of type="range".
 *
 * @tparam V
 */
trait InputRangeStyler[V] {
  def apply(lb: V, ub: V, sc: SimpleConverter[V]): AttrsT = Attrs ~= (AttrDescs.type_@, "range") ~= (AttrDescs.min_@, sc.format(lb)) ~= (AttrDescs.max_@, sc.format(ub)) ~= step(lb, ub, sc)
  protected def step(lb: V, ub: V, sc: SimpleConverter[V]): Option[Attr]
}