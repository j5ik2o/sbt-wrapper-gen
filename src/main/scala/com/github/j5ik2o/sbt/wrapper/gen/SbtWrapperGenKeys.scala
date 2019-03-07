package com.github.j5ik2o.sbt.wrapper.gen

import com.github.j5ik2o.sbt.wrapper.gen.model.TypeDesc
import com.github.javaparser.ParserConfiguration
import sbt._

trait SbtWrapperGenKeys {

  val scalaWrapperGen = taskKey[Unit]("sbt-wrapper-gen key")

  val typeDescFilter          = settingKey[TypeDesc => Boolean]("")
  val typeDescMapper          = settingKey[(String, Seq[TypeDesc]) => TypeDesc]("")
  val templateDirectory       = settingKey[File]("")
  val templateNameMapper      = settingKey[TypeDesc => String]("")
  val inputDirectory          = settingKey[File]("")
  val outputDirectoryMapper   = settingKey[TypeDesc => File]("")
  val javaParserConfiguration = settingKey[Option[ParserConfiguration]]("")

  val generateOne  = inputKey[Seq[File]]("generate-one")
  val generateMany = inputKey[Seq[File]]("generate-many")
  val generateAll  = inputKey[Seq[File]]("generate-all")

  val enableManagedClassPath = settingKey[Boolean]("enable-managed-class-path")

}

object SbtWrapperGenKeys extends SbtWrapperGenKeys
