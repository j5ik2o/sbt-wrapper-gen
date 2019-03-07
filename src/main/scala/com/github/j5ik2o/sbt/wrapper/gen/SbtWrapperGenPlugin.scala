package com.github.j5ik2o.sbt.wrapper.gen

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object SbtWrapperGenPlugin extends AutoPlugin with WrapperGen {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = JvmPlugin

  object autoImport extends SbtWrapperGenKeys

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    enableManagedClassPath in scalaWrapperGen := true,
    templateDirectory in scalaWrapperGen := baseDirectory.value / "sbt-wrapper-gen",
    templateNameMapper in scalaWrapperGen := { _ =>
      "template.ftl"
    },
    inputDirectory := baseDirectory.value / "sbt-wrapper-gen" / "java",
    outputDirectoryMapper in scalaWrapperGen := { _ =>
      (sourceManaged in Compile).value
    },
    javaParserConfiguration := None,
    typeDescFilter := { _ =>
      true
    },
    typeDescMapper in scalaWrapperGen := WrapperGen.defaultTypeDescMapper,
    generateOne in scalaWrapperGen := generateOneTask.evaluated
  )
}
