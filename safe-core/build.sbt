name := "safe"

libraryDependencies ++= Seq(
  "org.scalanlp"      %% "breeze"       % "0.5-SNAPSHOT",
  "com.github.scopt"  %% "scopt"        % "3.1.0",
  "com.typesafe.akka" %% "akka-actor"   % "2.2.1",
  "com.typesafe.akka" %% "akka-testkit" % "2.2.1"
)