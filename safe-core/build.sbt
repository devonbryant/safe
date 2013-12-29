name := "safe"

mainClass in Compile := Some("safe.Safe")

com.typesafe.sbt.SbtStartScript.StartScriptKeys.startScriptName <<= target / "run"

libraryDependencies ++= Seq(
  "org.scalanlp"         %% "breeze"       % "0.5.2",
  "com.github.scopt"     %% "scopt"        % "3.1.0",
  "com.typesafe.akka"    %% "akka-actor"   % "2.3-M2",
  "com.typesafe.akka"    %% "akka-testkit" % "2.3-M2",
  "com.codahale.metrics"  % "metrics-core" % "3.0.1"
)