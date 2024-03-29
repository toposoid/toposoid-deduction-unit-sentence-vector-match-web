name := """toposoid-deduction-unit-sentence-vector-match-web"""
organization := "com.linked.ideal"
version := "0.5"

lazy val root = (project in file(".")).enablePlugins(PlayScala)
organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

scalaVersion := "2.13.11"

libraryDependencies += guice
libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.5"
libraryDependencies += "com.ideal.linked" %% "toposoid-common" % "0.5"
libraryDependencies += "com.ideal.linked" %% "toposoid-knowledgebase-model" % "0.5"
libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-protocol-model" % "0.5"
libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-common" % "0.5"
libraryDependencies += "com.ideal.linked" %% "toposoid-feature-vectorizer" % "0.5"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test
libraryDependencies += "com.ideal.linked" %% "toposoid-sentence-transformer-neo4j" % "0.5"
libraryDependencies += "io.jvm.uuid" %% "scala-uuid" % "0.3.1" % Test

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.linked.ideal.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.linked.ideal.binders._"
