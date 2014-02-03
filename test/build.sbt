scalaVersion := "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0-RC2" % "test"

// autoCompilerPlugins := true

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)

scalacOptions += "-feature"

scalacOptions += "-language:higherKinds"

//libraryDependencies ++= Seq(
//  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
//)
