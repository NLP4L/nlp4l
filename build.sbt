name := """NLP4L"""

organization := "org.nlp4l"

version := "0.9-dev"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  "com.typesafe.play" %% "play-slick" % "1.1.1",
  "mysql" % "mysql-connector-java" % "5.1.45",
  "org.postgresql" % "postgresql" % "42.2.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "7.2.1",
  "org.apache.lucene" % "lucene-analyzers-kuromoji" % "7.2.1",
  "org.apache.lucene" % "lucene-suggest" % "7.2.1",
  "org.apache.lucene" % "lucene-backward-codecs" % "7.2.1",
  "org.apache.lucene" % "lucene-misc" % "7.2.1",
  "org.apache.solr" % "solr-core" % "7.2.1",
  "org.apache.solr" % "solr-solrj" % "7.2.1",
  "org.apache.opennlp" % "opennlp-tools" % "1.8.4",
  "org.apache.spark" %% "spark-core" % "1.6.3" exclude("org.slf4j", "slf4j-log4j12"),
  "org.apache.spark" %% "spark-mllib" % "1.6.3",
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
resolvers += "Restlet Repositories" at "http://maven.restlet.org"
resolvers += Resolver.url("Typesafe Ivy releases", url("https://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)

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

import com.typesafe.sbt.packager.Keys.scriptClasspath

scriptClasspath := {
  val originalClasspath = scriptClasspath.value
  val manifest = new java.util.jar.Manifest()
  manifest.getMainAttributes().putValue("Class-Path", originalClasspath.mkString(" "))
  val classpathJar = (target in Universal).value / "lib" / "classpath.jar"
  IO.jar(Seq.empty, classpathJar, manifest)
  Seq(classpathJar.getName)
}
mappings in Universal += (((target in Universal).value / "lib" / "classpath.jar") -> "lib/classpath.jar")

sources in doc in Compile := List()
