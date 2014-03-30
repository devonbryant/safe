name := "safe-dist"

mainClass in Compile := Some("safe.SafeCluster")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-cluster"  % "2.3.1",
  "org.apache.hadoop" %  "hadoop-client" % "2.2.0",
  "io.spray"          %  "spray-routing" % "1.3.1",
  "io.spray"          %  "spray-can"     % "1.3.1",
  "io.spray"          %% "spray-json"    % "1.2.5"
)