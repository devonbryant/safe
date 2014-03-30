import sbt._
import Keys._
import com.typesafe.sbt.SbtStartScript

object SafeBuild extends Build {
  val commonSettings = Seq(
    version := "0.2-SNAPSHOT",

    scalaVersion := "2.10.3",

    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Sonatype Releases"   at "http://oss.sonatype.org/content/repositories/releases",
      "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots/"),

    libraryDependencies ++= Seq(
      "org.scalatest"  % "scalatest_2.10" % "2.1.0"  % "test",
      "org.scalacheck" %% "scalacheck"    % "1.11.3" % "test"
    )
  )

  lazy val safeSettings = commonSettings ++ SbtStartScript.startScriptForClassesSettings

  lazy val root = Project(id = "safe-parent", base = file(".")).settings(commonSettings: _*) aggregate(core, distributed)

  lazy val core = Project(id = "safe", base = file("safe-core")).settings(safeSettings: _*)

  lazy val distributed = Project(id = "safe-dist", base = file("safe-dist")).settings(safeSettings: _*) dependsOn core
}