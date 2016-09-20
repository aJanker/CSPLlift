libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

libraryDependencies += "de.fosd.typechef" % "javabdd_repackaged" % "1.0b2"

libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5",
    "org.slf4j" % "slf4j-simple" % "1.7.5")

mainClass in Runtime := Some("de.fosd.typechef.cspllift.Launch")

//generate lift.sh file with full classpath
TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map { (base, cp, main) =>
  val template = """#!/bin/sh
java -ea -Xmx8g -Xms1g -Xss32m -classpath "%s" %s "$@"
"""
  val mainStr = main getOrElse sys.error("No main class specified")
  val contents = template.format(cp.files.absString, mainStr)
  val out = base / "../lift.sh"
  IO.write(out, contents)
  out.setExecutable(true)
  out
}

