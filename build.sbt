import sbt.Keys._

scalaVersion := "2.10.3"

lazy val plugin = project.settings(
  name := "plugin",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  publishArtifact in Compile := false
)

lazy val core = project.settings(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0-RC2" % "test",
      libraryDependencies += "org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full,
      resolvers += Resolver.sonatypeRepo("snapshots"),
      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full),
      scalacOptions += "-feature",
      scalacOptions += "-language:higherKinds",
      scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
       val addPlugin = "-Xplugin:" + jar.getAbsolutePath
       // add plugin timestamp to compiler options to trigger recompile of
       // main after editing the plugin. (Otherwise a 'clean' is needed.)
       val dummy = "-Jdummy=" + jar.lastModified
       Seq(addPlugin, dummy)
    }
  )

lazy val playMod = Project("play-mod", file("play-mod")).dependsOn(core).settings(
    name := "play-mod",
    version := "1.0-SNAPSHOT",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0-RC2" % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full),
    scalacOptions += "-feature",
    scalacOptions += "-language:higherKinds",
      scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  ).settings(play.Project.playScalaSettings: _*).settings(
    templatesImport += "eu.swdev.web.form._",
    templatesImport += "eu.swdev.web.style._",
    templatesImport += "eu.swdev.play.form.bootstrap3._"
  )

lazy val playApp = Project("play-app", file("play-app")).dependsOn(core, playMod).settings(
  name := "play-app",
  version := "1.0-SNAPSHOT",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0-RC2" % "test",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full),
  scalacOptions += "-feature",
  scalacOptions += "-language:higherKinds",
  scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
    val addPlugin = "-Xplugin:" + jar.getAbsolutePath
    // add plugin timestamp to compiler options to trigger recompile of
    // main after editing the plugin. (Otherwise a 'clean' is needed.)
    val dummy = "-Jdummy=" + jar.lastModified
    Seq(addPlugin, dummy)
  }
).settings(play.Project.playScalaSettings: _*).settings(
    templatesImport += "eu.swdev.web.form._",
    templatesImport += "eu.swdev.web.style._",
    templatesImport += "eu.swdev.play.form.bootstrap3._"
  )


lazy val root = project.in(file(".")).aggregate(core, playMod, playApp)