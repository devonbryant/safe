name := "safe"

libraryDependencies ++= Seq(
  "org.scalaz"        %% "scalaz-core"  % "7.0.0",
  "org.scalanlp"      %% "breeze-math"  % "0.4-SNAPSHOT",
  "com.typesafe.akka" %% "akka-actor"   % "2.1.4",
  "com.typesafe.akka" %% "akka-testkit" % "2.1.4"
)