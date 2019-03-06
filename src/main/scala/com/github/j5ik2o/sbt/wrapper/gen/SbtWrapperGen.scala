package com.github.j5ik2o.sbt.wrapper.gen

import com.github.j5ik2o.sbt.wrapper.gen.model.{TypeDesc, _}
import sbt._
import sbt.Keys._
import sbt.io._
import sbt.plugins.JvmPlugin

class SbtWrapperGen extends AutoPlugin with WrapperGen {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = JvmPlugin

  object autoImport extends SbtWrapperGenKeys

  import autoImport._
  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    inputDirectory in scalaWrapperGen := file("aws-sdk-src/aws-sdk-java-v2/services/dynamodb/target/generated-sources/sdk/software/amazon/awssdk/services/dynamodb"),
    outputDirectory in scalaWrapperGen := (sourceManaged in Compile).value,
    templateMapper in scalaWrapperGen := { _ =>
    "template.ftl"
  },
  typeMapper in scalaWrapperGen := ({
    case ("String", _) =>
      StringTypeDesc()
    case ("Array", Seq(paramType)) =>
      ArrayTypeDesc(paramType)
    case ("List", Seq(paramType)) =>
      SeqTypeDesc(paramType)
    case ("Map", params) =>
      MapTypeDesc(params(0), params(1))
    case ("CompletableFuture", Seq(paramType)) =>
      FutureDesc(paramType)
    case ("Integer", _) =>
      PrimitiveTypeDesc(PrimitiveType.INT)
//    case ("Log", _) =>
//      PrimitiveTypeDesc(PrimitiveType.LONG)
//    case ("Short", _) =>
//      PrimitiveTypeDesc(PrimitiveType.SHORT)
//    case ("Boolean", _) =>
//      PrimitiveTypeDesc(PrimitiveType.BOOLEAN)
//    case ("Byte", _) =>
//      PrimitiveTypeDesc(PrimitiveType.BYTE)
//    case ("Char", _) =>
//      PrimitiveTypeDesc(PrimitiveType.CHAR)
//    case ("Double", _) =>
//      PrimitiveTypeDesc(PrimitiveType.DOUBLE)
//    case ("Float", _) =>
//      PrimitiveTypeDesc(PrimitiveType.FLOAT)
//    case (other, _) =>
//      OtherTypeDesc(other)
  }: (String, Seq[TypeDesc]) => TypeDesc)
  )
//  generate in scalaWrapperGen  := {
//   generateWrapper((inputDirectory in scalaWrapperGen).value,
//      (outputDirectory in scalaWrapperGen).value,
//      (templateMapper in scalaWrapperGen).value,
//      (typeMapper in scalaWrapperGen).value)
//  }
}
