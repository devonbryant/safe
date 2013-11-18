name := "safe-dist"

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-cluster"  % "2.2.3",
  "io.spray"          %  "spray-routing" % "1.2-RC2",
  "io.spray"          %  "spray-can"     % "1.2-RC2",
  "io.spray"          %% "spray-json"    % "1.2.5"
)