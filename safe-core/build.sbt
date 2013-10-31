name := "safe"

com.typesafe.sbt.SbtStartScript.StartScriptKeys.startScriptName <<= target / "run"

libraryDependencies ++= Seq(
  "org.scalanlp"         %% "breeze"       % "0.5.2",
  "com.github.scopt"     %% "scopt"        % "3.1.0",
  "com.typesafe.akka"    %% "akka-actor"   % "2.2.1",
  "com.typesafe.akka"    %% "akka-testkit" % "2.2.1",
  "com.codahale.metrics"  % "metrics-core" % "3.0.1"
)