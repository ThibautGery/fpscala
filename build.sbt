name := "fpscala"

version := "1.0"

scalaVersion := "2.12.0"


libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.6" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

testOptions in Test ++= Seq( Tests.Argument("junitxml", "junit.outdir", sys.props.getOrElse("CIRCLE_TEST_REPORTS", default = "target/test-reports" )), Tests.Argument("console") )