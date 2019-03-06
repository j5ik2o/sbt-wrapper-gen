package com.github.j5ik2o.sbt.wrapper.gen.model

sealed trait TypeDesc {
  def name: String = asString
  def asString: String
}

case class PrimitiveTypeDesc(`type`: PrimitiveType) extends TypeDesc {
  override def asString: String = `type`.entryName
}

case class VoidTypeDesc() extends TypeDesc {
  override def asString: String = "Unit"
}

case class StringTypeDesc() extends TypeDesc {
  override def asString: String = "String"
}
