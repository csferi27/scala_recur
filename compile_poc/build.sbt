name := "Compile recursive POC"

version := "0.1"

val scalaBuildVersion = "2.10.3"

scalaVersion := scalaBuildVersion

scalacOptions += "-deprecation"

//Default
libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-compiler" % scalaBuildVersion,
	"org.scala-lang" % "scala-library" % scalaBuildVersion,
	"org.scala-lang" % "scala-reflect" % scalaBuildVersion,	
	"org.slf4j" % "slf4j-nop" % "1.6.4"
	)

// Test
libraryDependencies ++= Seq(		
	"org.scalatest" %% "scalatest" % "2.1.0" % "test",
	"junit" % "junit" % "4.11" % "test"
	)
	
testOptions in ThisBuild <+= (target in Test) map {
  t => Tests.Argument("-o", "-u", t + "/test-reports")
}


org.scalastyle.sbt.ScalastylePlugin.Settings

ScoverageSbtPlugin.instrumentSettings


