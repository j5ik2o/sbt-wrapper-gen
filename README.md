# sbt-wrapper-gen


This is the sbt plug-in for automatically generating Scala wrapper from Java source code.

## How to use plugin

Add this to your project/plugins.sbt file:

```scala
resolvers += "Sonatype OSS Release Repository" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.github.j5ik2o" % "sbt-wrapper-gen" % "1.0.0")
```

## How to configuration


If you like the default setting, it is zero configuration.

Add this to your build.sbt file:

```sh
templateDirectory in scalaWrapperGen := baseDirectory.value / "sbt-wrapper-gen"

templateNameMapper in scalaWrapperGen := { _ =>
      "template.ftl"
}

inputDirectory := baseDirectory.value / "sbt-wrapper-gen" / "java",
outputDirectoryMapper in scalaWrapperGen := { _ =>
  (sourceManaged in Compile).value
}

typeDescFilter := { case typeDesc =>
      true
}
```