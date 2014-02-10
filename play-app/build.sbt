name := "play-form-example-app"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache 
)     

resolvers += Resolver.file("LocalIvy", file(Path.userHome +
      java.io.File.separator + ".ivy2" + java.io.File.separator +
      "local"))(Resolver.ivyStylePatterns)
      
libraryDependencies += "core" %% "core" % "0.1-SNAPSHOT"

play.Project.playScalaSettings

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)

scalacOptions += "-feature"

scalacOptions += "-language:higherKinds"


