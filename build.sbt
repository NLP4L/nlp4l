name := """NLP4L"""

organization := "org.nlp4l"

version := "0.6.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  "com.typesafe.play" %% "play-slick" % "1.1.0",
  "mysql" % "mysql-connector-java" % "5.1.38",
  "org.postgresql" % "postgresql" % "9.4-1206-jdbc41",
  "org.apache.lucene" % "lucene-analyzers-common" % "5.4.1",
  "org.apache.lucene" % "lucene-analyzers-kuromoji" % "5.4.1",
  "org.apache.lucene" % "lucene-suggest" % "5.4.1",
  "org.apache.lucene" % "lucene-backward-codecs" % "5.4.1",
  "org.apache.solr" % "solr-solrj" % "5.4.1",
  "org.apache.opennlp" % "opennlp-tools" % "1.6.0",
  "org.apache.spark" %% "spark-core" % "1.6.1" exclude("org.slf4j", "slf4j-log4j12"),
  "org.apache.spark" %% "spark-mllib" % "1.6.1",
  "org.apache.commons" % "commons-csv" % "1.4",
  "com.jsuereth" %% "scala-arm" % "1.4",
  "org.webjars" %% "webjars-play" % "2.4.0-1",
  "org.webjars" % "bootstrap" % "3.3.5",
  "com.github.tototoshi" %% "slick-joda-mapper" % "2.1.0",
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.7",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.3",
  "com.github.scala-incubator.io" % "scala-io-core_2.11" % "0.4.3",
  "com.github.scala-incubator.io" % "scala-io-file_2.11" % "0.4.3",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)

dependencyOverrides ++= Set(
  // Spark is expecting jackson-databind 2.4.4
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.4"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

unmanagedBase := baseDirectory.value / "lib"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator

mappings in Universal ++=
  (baseDirectory.value / "examples" * "*" get) map
    (x => x -> ("examples/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "conf/application.conf.sample" get) map
    (x => x -> ("conf/application.conf"))

