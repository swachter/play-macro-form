package eu.swdev.play.form

/**
  */
trait Mapping {

  type Value

  def get(implicit formState: FormState[_]): Value
  def set(v: Value)(implicit formState: FormState[_]): Unit

  def fill(map: Map[String, Seq[String]])(implicit formState: FormState[_]): Unit

}
