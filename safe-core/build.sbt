name := "safe-core"

version := "0.1"

scalaVersion := "2.10.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0-M7",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)