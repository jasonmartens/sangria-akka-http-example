name := "sangria-akka-http-example"
version := "0.1.0-SNAPSHOT"

description := "An example GraphQL server written with akka-http and sangria."

scalaVersion := "2.12.4"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "1.4.1",
//  "org.sangria-graphql" %% "sangria-spray-json" % "1.0.1",

  "org.sangria-graphql" %% "sangria-circe" % "1.2.1",
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.1",
  "io.circe" %%	"circe-core" % "0.9.3",
  "io.circe" %% "circe-parser" % "0.9.3",
  "io.circe" %% "circe-optics" % "0.9.3",

  "com.typesafe.akka" %% "akka-http" % "10.0.10",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.10",

  "org.scalatest" %% "scalatest" % "3.0.4" % Test
)

Revolver.settings
enablePlugins(JavaAppPackaging)
