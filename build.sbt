name := "FooParBra"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.1"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

scalacOptions ++= Seq("-Yvirtualize","-feature")

javaOptions += "-server"
