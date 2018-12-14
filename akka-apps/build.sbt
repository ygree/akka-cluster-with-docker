
name := "akka-cluster-demo"

version := "1.0.3"

scalaVersion := "2.12.2"

lazy val akkaVersion = "2.5.19"

lazy val akkaManagementVersion = "0.20.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
  "com.lightbend.akka" %% "akka-split-brain-resolver" % "1.1.5",
  "com.lightbend.akka.management" %% "akka-management" % akkaManagementVersion,
  "com.lightbend.akka.management" %% "akka-management-cluster-http" % akkaManagementVersion,
  "ch.qos.logback" %  "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

mainClass in Compile := Some("simple.App")

//dockerBaseImage := "java-base:latest"
dockerBaseImage := "openjdk:alpine"

javaOptions in Universal ++= Seq(
  "-Dconfig.file=/configs/application.conf"
)

enablePlugins(AshScriptPlugin)

