name := "CSPLlift"

version := "1.0"

organization := "de.fosd.typechef"

scalaVersion := "2.11.7"

resolvers += "SonaType" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += Resolver.url("sbt-plugin-releases_", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

javacOptions ++= Seq("-Xlint:unchecked", "-target", "1.8", "-source", "1.8")

scalacOptions ++= Seq("-unchecked", "-target:jvm-1.8")
   
testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

libraryDependencies += "de.fosd.typechef" % "frontend_2.11" % "0.4.1"

mainClass in Runtime := Some("de.fosd.typechef.cspllift.CSPLliftFrontend")

//generate lift.sh file with full classpath
TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map { (base, cp, main) =>
  val template = """#!/bin/sh
java -ea -Xmx8g -Xms1g -Xss32m -classpath "%s" %s "$@"
"""
  val mainStr = main getOrElse sys.error("No main class specified")
  val contents = template.format(cp.files.absString, mainStr)
  val out = base / "morpheus.sh"
  IO.write(out, contents)
  out.setExecutable(true)
  out
}

