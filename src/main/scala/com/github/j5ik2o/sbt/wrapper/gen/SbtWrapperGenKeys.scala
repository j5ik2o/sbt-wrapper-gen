package com.github.j5ik2o.sbt.wrapper.gen

import com.github.j5ik2o.sbt.wrapper.gen.model.TypeDesc
import sbt._

trait SbtWrapperGenKeys {
  val scalaWrapperGen = taskKey[Unit]("")

  val typeMapper = settingKey[(String, Seq[TypeDesc]) => TypeDesc]("")
  val templateMapper = settingKey[String => String]("")
  val inputDirectory  = settingKey[File]("")
  val outputDirectory = settingKey[File]("")
  val generate = taskKey[Unit]("")

}
