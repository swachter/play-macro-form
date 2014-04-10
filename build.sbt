import sbt.Keys._

scalaVersion := "2.10.3"

val scalaMacroVersion = "2.0.0-M3"

lazy val plugin = project.settings(
  name := "plugin",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  publishArtifact in Compile := false
)

val commonSettings = Seq(
    organization := "eu.swdev.playext",
    version := "0.1-SNAPSHOT",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.0" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    addCompilerPlugin("org.scalamacros" % "paradise" % scalaMacroVersion cross CrossVersion.full),
    scalacOptions += "-feature",
    scalacOptions += "-language:higherKinds"
    /*
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
    */
  ) ++ bintraySettings ++ Seq(
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    publishMavenStyle := true,
    bintray.Keys.repository in bintray.Keys.bintray := "generic",
    bintray.Keys.bintrayOrganization in bintray.Keys.bintray := None,
    bintray.Keys.packageLabels in bintray.Keys.bintray := Seq("Scala", "Play")
  )

val commonPlaySettings = commonSettings ++ play.Project.playScalaSettings

lazy val core = project.settings(commonSettings: _*).settings(
    libraryDependencies += "org.scalamacros" % "quasiquotes" % scalaMacroVersion cross CrossVersion.full,
    // macros that are executed when tests are compiled may need to access test resources
    // -> add the test resource directory to the test dependency classpath
    dependencyClasspath in Test ++= (unmanagedResourceDirectories in Test).value
  )

lazy val playMod = Project("play-mod", file("play-mod")).dependsOn(core).settings(
    name := "play-mod"
  ).settings(commonPlaySettings: _*).settings(
    templatesImport += "eu.swdev.web.form._",
    templatesImport += "eu.swdev.web.style._",
    templatesImport += "eu.swdev.web.style.AttrDescs._",
    templatesImport += "eu.swdev.play.form.MsgLookup",
    templatesImport += "eu.swdev.play.form.bootstrap3._"
  )

lazy val playApp = Project("play-app", file("play-app")).dependsOn(core, playMod).settings(
    name := "play-app"
  ).settings(commonPlaySettings: _*).settings(
    templatesImport += "eu.swdev.play.form.bootstrap3.Import._"
  )


lazy val root = project.in(file(".")).aggregate(core, playMod, playApp)