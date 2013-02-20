import sbt._
import Keys._

object SafeBuild extends Build {
  val commonSettings = Seq(
    version := "0.1",

    scalaVersion := "2.10.0",

    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",

    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
    )
  )

  lazy val root = Project(id = "safe", base = file(".")).settings(commonSettings: _*) aggregate(core, akka)

  lazy val core = Project(id = "safe-core", base = file("safe-core")).settings(commonSettings: _*)

  lazy val akka = Project(id = "safe-akka", base = file("safe-akka")).settings(commonSettings: _*) dependsOn core
}