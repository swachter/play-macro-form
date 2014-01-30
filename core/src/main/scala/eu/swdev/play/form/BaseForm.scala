package eu.swdev.play.form

import scala.reflect.runtime.{ universe => ru }

trait BaseForm extends Mapping {

  val fieldNames: Map[Any, String]

  def fieldName[C[_]](field: Field[_, C, _, _, _]) = fieldNames(field)

}
