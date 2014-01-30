package eu.swdev.play.form

/**
  */
abstract class FieldHandler[M, C[_]] {

  def fillFromView(view: Seq[String], fieldState: FieldState[C[M]]): Unit

  def fillFromModel(model: C[M], fieldState: FieldState[C[M]]): Unit

}
