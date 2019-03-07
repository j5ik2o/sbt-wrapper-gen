package com.github.j5ik2o.sbt.wrapper.gen.model

import java.util
import scala.collection.JavaConverters._

sealed trait Ast {
  def asString: String
  def asMap: util.Map[String, AnyRef]
}

sealed trait TypeDesc extends Ast {
  def simpleTypeName: String
  def fullTypeName: String
}

case class PrimitiveTypeDesc(primitiveType: PrimitiveType) extends TypeDesc {
  override def simpleTypeName: String          = primitiveType.entryName
  override def fullTypeName: String            = simpleTypeName
  override def asString: String                = simpleTypeName
  override def asMap: util.Map[String, AnyRef] = Map[String, AnyRef]("simpleTypeName" -> simpleTypeName).asJava
}

case class VoidTypeDesc() extends TypeDesc {
  override def simpleTypeName: String          = "Unit"
  override def fullTypeName: String            = simpleTypeName
  override def asString: String                = simpleTypeName
  override def asMap: util.Map[String, AnyRef] = Map[String, AnyRef]("simpleTypeName" -> simpleTypeName).asJava
}

case class StringTypeDesc() extends TypeDesc {
  override def simpleTypeName: String          = "String"
  override def fullTypeName: String            = simpleTypeName
  override def asString: String                = simpleTypeName
  override def asMap: util.Map[String, AnyRef] = Map[String, AnyRef]("simpleTypeName" -> simpleTypeName).asJava
}

case class MapTypeDesc(keyTypeName: TypeDesc, valueTypeName: TypeDesc) extends TypeDesc {
  override def simpleTypeName: String = "Map"
  override def fullTypeName: String   = s"Map[${keyTypeName.asString}, ${valueTypeName.asString}]"
  override def asString: String =
    s"Map[${keyTypeName.asString}, ${valueTypeName.asString}]"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef](
      "simpleTypeName" -> simpleTypeName,
      "keyTypeName"    -> keyTypeName.simpleTypeName,
      "valueTypeName"  -> valueTypeName.simpleTypeName,
      "fullTypeName"   -> fullTypeName
    ).asJava
}

case class OtherTypeDesc(typeName: String, typeParameters: Seq[TypeDesc] = Seq.empty) extends TypeDesc {
  override def simpleTypeName: String = typeName
  override def fullTypeName: String   = s"$simpleTypeName${typeParameters.map(_.simpleTypeName).mkString("[", ",", "]")}"
  override def asString: String       = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    (Map[String, AnyRef](
      "simpleTypeName" -> simpleTypeName
    ) ++ (if (typeParameters.nonEmpty) Map("typeParameters" -> typeParameters.map(_.asMap).asJava) else Map.empty)).asJava
}

case class FutureDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def simpleTypeName: String = s"Future"
  override def fullTypeName: String   = s"Future[${valueTypeName.asString}]"
  override def asString: String       = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "valueTypeName" -> valueTypeName.simpleTypeName).asJava
}

case class SeqTypeDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def simpleTypeName: String = "Seq"
  override def fullTypeName: String   = s"Seq[${valueTypeName.asString}]"
  override def asString: String       = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "valueTypeName" -> valueTypeName.simpleTypeName).asJava
}

case class ArrayTypeDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def simpleTypeName: String = "Array"
  override def fullTypeName: String   = s"Array[${valueTypeName.asString}]"
  override def asString: String       = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "valueTypeName" -> valueTypeName.simpleTypeName).asJava
}

case class ParameterTypeDesc(name: String, typeName: TypeDesc) extends Ast {
  override def asString: String = s"$name: ${typeName.asString}"
  override def asMap: util.Map[String, AnyRef] =
    Map("name" -> name, "typeName" -> typeName.asMap).asJava
}

case class ConstructorDesc(parameters: Seq[ParameterTypeDesc]) extends Ast {
  override def asString: String =
    "(" + parameters.map(_.asString).mkString(",") + ")"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("parameters" -> parameters.map(_.asMap).asJava).asJava
}

case class MethodDesc(name: String, parameters: Seq[ParameterTypeDesc], returnType: TypeDesc) extends Ast {
  override def asString: String =
    s"def $name(${parameters.map(_.asString).mkString(",")}): ${returnType.asString}"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("name"       -> name,
                        "parameters" -> parameters.map(_.asMap).asJava,
                        "returnType" -> returnType.asMap).asJava
}

case class ClassDesc(simpleTypeName: String,
                     constructor: ConstructorDesc,
                     methods: Seq[MethodDesc],
                     packageName: Option[String] = None)
    extends TypeDesc {
  override def fullTypeName: String = simpleTypeName
  override def asString: String = {
    s"class ${simpleTypeName}${Option(constructor).map(_.asString).getOrElse("")} {" +
    methods.map(_.asString).mkString("\n\n  ", "\n\n  ", "\n\n") +
    "}"
  }
  override def asMap: util.Map[String, AnyRef] =
    (Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "methods" -> methods.map(_.asMap).asJava) ++
    (if (packageName.nonEmpty) Map("packageName" -> packageName.get) else Map.empty)).asJava
}
