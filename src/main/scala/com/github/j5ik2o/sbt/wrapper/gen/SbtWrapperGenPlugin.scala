package com.github.j5ik2o.sbt.wrapper.gen

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object SbtWrapperGenPlugin extends AutoPlugin with WrapperGenerator {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = JvmPlugin

  object autoImport extends SbtWrapperGenKeys

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    templateDirectory in scalaWrapperGen := baseDirectory.value / "sbt-wrapper-gen",
    templateNameMapper in scalaWrapperGen := { _ =>
      "template.ftl"
    },
    inputSourceDirectory in scalaWrapperGen := baseDirectory.value / "sbt-wrapper-gen" / "java",
    outputSourceDirectoryMapper in scalaWrapperGen := { _ =>
      (sourceManaged in Compile).value
    },
    typeNameMapper in scalaWrapperGen := { cd =>
      Seq(cd.simpleTypeName)
    },
    packageNameMapper in scalaWrapperGen := { identity },
    javaParserConfiguration in scalaWrapperGen := None,
    typeDescFilter in scalaWrapperGen := { _ =>
      true
    },
    typeDescMapper in scalaWrapperGen := WrapperGenerator.defaultTypeDescMapper,
    generateOne in scalaWrapperGen := generateOneTask.evaluated,
    generateMany in scalaWrapperGen := generateManyTask.evaluated,
    generateAll in scalaWrapperGen := generateAllTask.value
  )
}
