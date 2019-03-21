package com.github.j5ik2o.sbt.wrapper.gen

import com.github.j5ik2o.sbt.wrapper.gen.model.TypeDesc
import com.github.javaparser.ParserConfiguration
import sbt._

trait SbtWrapperGenKeys {

  val scalaWrapperGen = taskKey[Unit]("sbt-wrapper-gen key")

  val typeDescFilter              = settingKey[TypeDesc => Boolean]("class desc filter")
  val typeDescMapper              = settingKey[(String, Seq[TypeDesc], Option[String]) => TypeDesc]("type desc mapper")
  val templateDirectory           = settingKey[File]("template directory")
  val templateNameMapper          = settingKey[TypeDesc => String]("template name mapper")
  val inputSourceDirectory        = settingKey[File]("input source directory")
  val outputSourceDirectoryMapper = settingKey[TypeDesc => File]("output source directory")
  val typeNameMapper              = settingKey[TypeDesc => String]("type name mapper")
  val packageNameMapper           = settingKey[String => String]("package name mapper")
  val javaParserConfiguration     = settingKey[Option[ParserConfiguration]]("java parser configuration")

  val generateOne  = inputKey[Seq[File]]("generate-one task")
  val generateMany = inputKey[Seq[File]]("generate-many task")
  val generateAll  = inputKey[Seq[File]]("generate-all task")

}

object SbtWrapperGenKeys extends SbtWrapperGenKeys
