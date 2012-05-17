name := "pcomb"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "net.databinder" %% "dispatch-core" % "0.8.7",
  "net.databinder" %% "dispatch-oauth" % "0.8.7",
  "net.databinder" %% "dispatch-nio" % "0.8.7",
  "net.databinder" %% "dispatch-http" % "0.8.7",
  "net.databinder" %% "dispatch-tagsoup" % "0.8.7",
  "net.databinder" %% "dispatch-jsoup" % "0.8.7",
  "net.databinder" %% "unfiltered-filter" % "0.6.1",
  "net.databinder" %% "unfiltered-netty" % "0.6.1",
  "net.databinder" %% "unfiltered-netty-server" % "0.6.1",
  "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://n0d.es/jars/caliper-1.0-SNAPSHOT.jar",
  "com.azavea.math" %% "numeric" % "0.1" from "http://n0d.es/jars/numeric_2.9.1-0.1.jar",
  "com.azavea.math.plugin" %% "optimized-numeric" % "0.1" from "http://plastic-idolatry.com/jars/optimized-numeric-plugin_2.9.1-0.1.jar",
  "azavea" %% "geotrellis" % "0.6",
  "org.clapper" %% "avsl" % "0.3.6")


resolvers ++= Seq(
      "Geotools" at "http://download.osgeo.org/webdav/geotools/", 
      "maven2 dev repository" at "http://download.java.net/maven/2",
      "Scala Test" at "http://www.scala-tools.org/repo-reloases/",
      "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
      "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
      "sonatypeSnapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
    )
