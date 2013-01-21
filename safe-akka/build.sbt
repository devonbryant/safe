name := "safe-akka"

version := "0.1"

scalaVersion := "2.10.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.1.0",
  "com.typesafe.akka" %% "akka-testkit" % "2.1.0",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)