package com.github.j5ik2o.sbt.wrapper.gen.model

sealed trait TypeDesc {
  def name: String
  def asString: String
  def asMap: Map[String, AnyRef] = Map("typeName" -> name)
}

case class PrimitiveTypeDesc(primitiveType: PrimitiveType) extends TypeDesc {
  override def name: String = primitiveType.entryName
  override def asString: String = name
}

case class VoidTypeDesc() extends TypeDesc {
  override def name: String = "Unit"
  override def asString: String = name
}

case class StringTypeDesc() extends TypeDesc {
  override def name: String = "String"
  override def asString: String = name
}

case class MapTypeDesc(keyTypeName: TypeDesc, valueTypeName: TypeDesc)
  extends TypeDesc {
  override def name: String = "Map"
  override def asString: String =
    s"Map[${keyTypeName.asString}, ${valueTypeName.asString}]"
  override def asMap: Map[String, AnyRef] =
    super.asMap ++ Map(
      "keyTypeName" -> keyTypeName.name,
      "valueTypeName" -> valueTypeName.name
    )
}

case class OtherTypeDesc(typeName: String, typeParameters: Seq[TypeDesc] = Seq.empty)  extends TypeDesc {
  override def name: String = typeName
  override def asString: String = s"$name${typeParameters.map(_.name).mkString("[", ",", "]")}"
}

case class FutureDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def name: String = "Future"
  override def asString: String = s"Future[${valueTypeName.asString}]"
  override def asMap: Map[String, AnyRef] = super.asMap ++ Map("valueTypeName" -> valueTypeName.name)
}

case class SeqTypeDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def name: String = "Seq"
  override def asString: String = s"Seq[${valueTypeName.asString}]"
}

case class ArrayTypeDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def name: String = "Seq"
  override def asString: String = s"Seq[${valueTypeName.asString}]"
}