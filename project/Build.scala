import sbt._
import Keys._

object FormBuild extends Build {

  lazy val core = project

  lazy val macro = project.dependsOn(core)

  lazy val test = project.dependsOn(core, macro) 

}
