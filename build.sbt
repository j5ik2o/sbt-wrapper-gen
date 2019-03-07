import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
import xerial.sbt.Sonatype.autoImport._

releaseCrossBuild := true

releasePublishArtifactsAction := PgpKeys.publishSigned.value

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("^ publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)

sonatypeProfileName := "com.github.j5ik2o"

organization := "com.github.j5ik2o"

publishMavenStyle := true

publishArtifact in Test := false

publishTo := sonatypePublishTo.value

pomIncludeRepository := {
  _ => false
}

pomExtra := {
  <url>https://github.com/septeni-original/sbt-dao-generator</url>
    <licenses>
      <license>
        <name>The MIT License</name>
        <url>http://opensource.org/licenses/MIT</url>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:j5ik2o/sbt-wrapper-gen.git</url>
      <connection>scm:git:github.com/j5ik2o/sbt-wrapper-gen</connection>
      <developerConnection>scm:git:git@github.com:j5ik2o/sbt-wrapper-gen.git</developerConnection>
    </scm>
    <developers>
      <developer>
        <id>j5ik2o</id>
        <name>Junichi Kato</name>
      </developer>
    </developers>
}


name := "sbt-wrapper-gen"

sbtPlugin := true

val sbtCrossVersion = sbtVersion in pluginCrossBuild

scalaVersion := (CrossVersion partialVersion sbtCrossVersion.value match {
  case Some((0, 13)) => "2.10.6"
  case Some((1, _)) => "2.12.8"
  case _ => sys error s"Unhandled sbt version ${sbtCrossVersion.value}"
})

crossSbtVersions := Seq("0.13.16", "1.0.4")

resolvers ++= Seq(
  "Sonatype OSS Snapshot Repository" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype OSS Release Repository" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
  "Seasar Repository" at "http://maven.seasar.org/maven2/"
)


libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.freemarker" % "freemarker" % "2.3.22",
  "org.seasar.util" % "s2util" % "0.0.1",
  "com.github.javaparser"    % "javaparser-core"     % "3.13.1",
  "com.google.code.findbugs" % "jsr305"              % "3.0.2",
  "com.beachape"             %% "enumeratum"         % "1.5.13",
  "org.scala-lang.modules"   %% "scala-java8-compat" % "0.9.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "com.h2database" % "h2" % "1.4.187" % Test
)

credentials += Credentials((baseDirectory in LocalRootProject).value / ".credentials")

scriptedBufferLog := false

scriptedLaunchOpts := {
  scriptedLaunchOpts.value ++
    Seq("-Xmx1024M", "-Dproject.version=" + version.value)
}

scriptedBufferLog := false