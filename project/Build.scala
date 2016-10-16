// https://github.com/harrah/xsbt/wiki/Full-Configuration-Example

import java.text.SimpleDateFormat
import java.util.Date

import sbt.Keys._
import sbt._

object BuildSettings {

  import Dependencies._

    val buildOrganization = "de.fosd.typechef"
    val buildVersion = "0.1.0"
    val buildScalaVersion = "2.11.8"


  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt := ShellPrompt.buildShellPrompt,

    javacOptions ++= Seq("-Xlint:unchecked", "-target", "1.8", "-source", "1.8"),
    scalacOptions ++= Seq("-unchecked", "-target:jvm-1.8"),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),

    // suppress feature warnings in Scala 2.10.x
    // (we do not actually change the code to allow cross builds with prior scala versions)
    scalacOptions <++= scalaVersion map {
      sv =>
        if (sv startsWith "2.1") List(
          //"-Yinline-warnings",
          "-feature",
          "-language:postfixOps",
          "-language:implicitConversions"
          // "-Xfatal-warnings" // make sure we take warnings seriously
        )
        else Nil
    },

    conflictWarning := ConflictWarning.disable,

    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, add dependency on scala-xml module
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value ++ testEnvironment ++ scala211Libraries ++ typechefLib
        case _ => libraryDependencies.value ++ testEnvironment ++ typechefLib
      }
    },

    parallelExecution := false, //run into memory problems on hudson otherwise

    homepage := Some(url("https://github.com/janker/CSPLlift")),
    licenses := Seq("GNU Lesser General Public License v3.0" -> url("http://www.gnu.org/licenses/lgpl.txt")),

    //maven
    publishTo <<= version {
      (v: String) =>
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := {
      _ => false
    }
  )
}

object ShellPrompt {

  object devnull extends ProcessLogger {
    def info(s: => String) {}

    def error(s: => String) {}

    def buffer[T](f: => T): T = f
  }

  val current = """\*\s+(\w+)""".r

  def gitBranches = (("git branch --no-color" lines_! devnull).mkString)

  val buildShellPrompt = {
    (state: State) => {
      val currBranch =
        current findFirstMatchIn gitBranches map (_ group (1)) getOrElse "-"
      val currProject = Project.extract(state).currentProject.id
      "%s:%s:%s> ".format(
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }

}

object Dependencies {
    val junit = "junit" % "junit" % "4.12" % "test"
    val junitInterface = "com.novocode" % "junit-interface" % "0.11" % "test"
    val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    val scalatest = "org.scalatest" %% "scalatest" % "2.2.5" % "test"
    val scalaparsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
    val scalaxml = "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
    val typechef =  "de.fosd.typechef" %% "frontend" % "0.4.1_lift"

  val testEnvironment = Seq(junit, junitInterface, scalatest, scalacheck)
  val scala211Libraries = Seq(scalaparsercombinators, scalaxml)
  val typechefLib = Seq(typechef)
}

object VersionGen {

  object VersionGenKeys {
    val versionGen = TaskKey[Seq[File]]("version-gen")
    val versionGenPackage = SettingKey[String]("version-gen-package")
    val versionGenClass = SettingKey[String]("version-gen-class")
  }

  import VersionGen.VersionGenKeys._

  lazy val settings = Seq(
    versionGenPackage <<= Keys.organization {
      (org) => org
    },
    versionGenClass := "Version",
    versionGen <<= (sourceManaged in Compile, name, version, versionGenPackage, versionGenClass) map {
      (sourceManaged: File, name: String, version: String, vgp: String, vgc: String) =>
        val file = sourceManaged / vgp.replace(".", "/") / ("%s.scala" format vgc)
        val code =
          (
            if (vgp != null && vgp.nonEmpty) "package " + vgp + "\n"
            else ""
            ) +
            "class " + vgc + " extends VersionInfo {\n" +
            "  def getVersion:String = \"" + version + "_" + new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()) + "\"\n" +
            "}\n"
        IO write(file, code)
        Seq(file)
    },
    sourceGenerators in Compile <+= versionGen map identity
  )
}

object SPLlift extends Build {

  import BuildSettings._

  lazy val spllift = Project(
    "TypeChef-SPLlift",
    file("."),
    settings = buildSettings
  ) aggregate(customization,
    cpointeranalysis,
    cspllift
    )

  lazy val customization = Project(
    "Customization",
    file("Customization"),
    settings = buildSettings
  )

  lazy val cpointeranalysis = Project(
    "CPointerAnalysis",
    file("CPointerAnalysis"),
    settings = buildSettings
  ) dependsOn(customization)

  lazy val cspllift = Project(
    "CSPLlift",
    file("CSPLlift"),
    settings = buildSettings
  ) dependsOn(cpointeranalysis, customization)

}


