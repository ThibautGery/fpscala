name := "function programming in scala part 1"

scalaVersion := "2.11.5"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.0" % "test"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.0" % "test"
libraryDependencies += "org.specs2" %% "specs2-matcher-extra" % "3.0" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions ++= Seq("-Yrangepos", "-deprecation", "-unchecked", "-feature", "-language:_")
