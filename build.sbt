scalaVersion := "2.10.3"


lazy val core = project.settings(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0-RC2" % "test",
      libraryDependencies += "org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full,
      resolvers += Resolver.sonatypeRepo("snapshots"),
      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full),
      scalacOptions += "-feature",
      scalacOptions += "-language:higherKinds"
  )

lazy val playApp = Project("playApp", file("play-app")).dependsOn(core).settings(
    name := "play-form-example-app",
    version := "1.0-SNAPSHOT",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full),
    scalacOptions += "-feature",
    scalacOptions += "-language:higherKinds"
  ).settings(play.Project.playScalaSettings: _*)


lazy val root = project.in(file(".")).aggregate(core, playApp)