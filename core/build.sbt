scalaVersion := "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.RC1" % "test"

scalacOptions += "-feature"

scalacOptions += "-language:higherKinds"

//libraryDependencies ++= Seq(
//  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
//)
