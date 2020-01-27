name := "lista9-parallel"

version := "0.1"

scalaVersion := "2.13.1"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

// https://mvnrepository.com/artifact/com.storm-enroute/scalameter
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.19"