import xerial.sbt.Sonatype._

publishMavenStyle := true


sonatypeProfileName := "com.github.j5ik2o"
sonatypeProjectHosting := Some(GitHubHosting(user="j5ik2o", repository="sbt-wrapper-gen", email="j5ik2o@gmail.com"))
developers := List(
  Developer(id = "j5ik2o", name = "Junichi Kato", email = "j5ik2o", url = url("http://blog.j5ik2o.me"))
)
licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

publishTo := sonatypePublishTo.value