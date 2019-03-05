package com.github.j5ik2o.sbt.wrapper.gen.model

import enumeratum._

import scala.collection.immutable

sealed abstract class PrimitiveType(override val entryName: String) extends EnumEntry

object PrimitiveType extends Enum[PrimitiveType] {
  override def values: immutable.IndexedSeq[PrimitiveType] = findValues

  case object BOOLEAN extends PrimitiveType("Boolean")

  case object CHAR extends PrimitiveType("Character")

  case object BYTE extends PrimitiveType("Byte")

  case object SHORT extends PrimitiveType("Short")

  case object INT extends PrimitiveType("Int")

  case object LONG extends PrimitiveType("Long")

  case object FLOAT extends PrimitiveType("Float")

  case object DOUBLE extends PrimitiveType("Double")
}
