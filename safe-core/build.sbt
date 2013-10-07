name := "safe"

libraryDependencies ++= Seq(
  "org.scalaz"        %% "scalaz-core"  % "7.0.3",
  "org.scalanlp"      %% "breeze-math"  % "0.5-SNAPSHOT",
  "com.github.scopt"  %% "scopt"        % "3.1.0",
  "com.typesafe.akka" %% "akka-actor"   % "2.2.0",
  "com.typesafe.akka" %% "akka-testkit" % "2.2.0"
)