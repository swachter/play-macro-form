import sbt._
import Keys._

object FormBuild extends Build {

  lazy val core = project

  lazy val test = project.dependsOn(core)

}
