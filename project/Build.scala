import sbt._
import Keys._

object SafeBuild extends Build {
  val commonSettings = Seq(
    version := "0.1",

    scalaVersion := "2.10.1",

    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Sonatype Releases"   at "http://oss.sonatype.org/content/repositories/releases",
      "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots/"),

    libraryDependencies ++= Seq(
      "org.scalatest"  % "scalatest_2.10" % "1.9.1"  % "test",
      "org.scalacheck" %% "scalacheck"    % "1.10.1" % "test"
    )
  )

  lazy val root = Project(id = "safe", base = file(".")).settings(commonSettings: _*) aggregate(core, akka)

  lazy val core = Project(id = "safe-core", base = file("safe-core")).settings(commonSettings: _*)

  lazy val akka = Project(id = "safe-akka", base = file("safe-akka")).settings(commonSettings: _*) dependsOn core
}