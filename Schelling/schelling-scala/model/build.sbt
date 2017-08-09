organization := "fr.iscpif"

name := "spacematters"

version := "1.0"

scalaVersion := "2.11.6"

osgiSettings

OsgiKeys.exportPackage := Seq("fr.iscpif.schelling.model.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("*")

scalariformSettings

resolvers += "ISC-PIF Release" at "http://maven.iscpif.fr/public/"

val monocleVersion = "1.1.1"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
)

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"
