package eu.swdev.web.play

trait CheckBoxValueInfo[V] {
  def uncheckedValue: String
  def checkedValue: String
  def isChecked(view: Seq[String]): Boolean = view.contains(checkedValue)
}

object CheckBoxValueInfo {

  implicit val booleanCheckBoxValueInfo = new CheckBoxValueInfo[Boolean] {
    override def checkedValue: String = "true"
    override def uncheckedValue: String = "false"
  }

}