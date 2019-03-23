package com.github.j5ik2o.sbt.wrapper.gen.model

import java.nio.file.Path
import java.util

import scala.collection.JavaConverters._

sealed trait Ast {
  type T <: Ast
  def asString: String
  def asMap: util.Map[String, AnyRef]
  def asScalaDesc: T
}

sealed trait TypeDesc extends Ast {
  override type T = TypeDesc
  def simpleTypeName: String
  def fullTypeName: String
  def packageName: Option[String]
}

case class PrimitiveTypeDesc(primitiveType: PrimitiveType) extends TypeDesc {
  override def packageName: Option[String] = None
  override def simpleTypeName: String      = primitiveType.entryName
  override def fullTypeName: String        = simpleTypeName
  override def asString: String            = simpleTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "fullTypeName" -> fullTypeName).asJava
  override def asScalaDesc: PrimitiveTypeDesc = this
}

case class UnitTypeDesc() extends TypeDesc {
  override def packageName: Option[String] = Some("scala")
  override def simpleTypeName: String      = "Unit"
  override def fullTypeName: String        = simpleTypeName
  override def asString: String            = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "fullTypeName" -> fullTypeName).asJava
  override def asScalaDesc: UnitTypeDesc = this
}

case class VoidTypeDesc() extends TypeDesc {
  override def packageName: Option[String] = None
  override def simpleTypeName: String      = "void"
  override def fullTypeName: String        = simpleTypeName
  override def asString: String            = simpleTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "fullTypeName" -> fullTypeName).asJava
  override def asScalaDesc: UnitTypeDesc = UnitTypeDesc()
}

case class StringTypeDesc() extends TypeDesc {
  override def packageName: Option[String] = Some("java.lang")
  override def simpleTypeName: String      = "String"
  override def fullTypeName: String        = simpleTypeName
  override def asString: String            = simpleTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName, "fullTypeName" -> fullTypeName).asJava
  override def asScalaDesc: StringTypeDesc = this
}

case class MapTypeDesc(keyTypeName: TypeDesc, valueTypeName: TypeDesc) extends TypeDesc {
  override def packageName: Option[String] = Some("scala.collection")
  override def simpleTypeName: String      = "Map"
  override def fullTypeName: String        = s"Map[${keyTypeName.asString}, ${valueTypeName.asString}]"
  override def asString: String =
    s"Map[${keyTypeName.asString}, ${valueTypeName.asString}]"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef](
      "simpleTypeName" -> simpleTypeName,
      "keyTypeName"    -> keyTypeName.simpleTypeName,
      "valueTypeName"  -> valueTypeName.simpleTypeName,
      "fullTypeName"   -> fullTypeName
    ).asJava
  override def asScalaDesc: MapTypeDesc = this
}

case class JavaMapTypeDesc(keyTypeName: TypeDesc, valueTypeName: TypeDesc) extends TypeDesc {
  override def packageName: Option[String] = Some("java.util")
  override def simpleTypeName: String      = "Map"
  override def fullTypeName: String        = s"Map[${keyTypeName.asString}, ${valueTypeName.asString}]"
  override def asString: String =
    s"Map[${keyTypeName.asString}, ${valueTypeName.asString}]"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef](
      "simpleTypeName" -> simpleTypeName,
      "keyTypeName"    -> keyTypeName.simpleTypeName,
      "valueTypeName"  -> valueTypeName.simpleTypeName,
      "fullTypeName"   -> fullTypeName
    ).asJava
  override def asScalaDesc: MapTypeDesc = MapTypeDesc(keyTypeName.asScalaDesc, valueTypeName.asScalaDesc)
}

case class WildcardTypeDesc(typeName: String) extends TypeDesc {
  override def packageName: Option[String]     = None
  override def simpleTypeName: String          = typeName
  override def fullTypeName: String            = simpleTypeName
  override def asString: String                = simpleTypeName
  override def asMap: util.Map[String, AnyRef] = Map[String, AnyRef]("wildcard" -> simpleTypeName).asJava
  override def asScalaDesc: WildcardTypeDesc   = this
}

case class OtherTypeDesc(typeName: String, typeParameters: Seq[TypeDesc], packageName: Option[String])
    extends TypeDesc {
  override def simpleTypeName: String = typeName
  override def fullTypeName: String =
    simpleTypeName + (if (typeParameters.nonEmpty) typeParameters.map(_.simpleTypeName).mkString("[", ",", "]") else "")
  override def asString: String = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    (Map[String, AnyRef](
      "simpleTypeName"    -> simpleTypeName,
      "fullTypeName"      -> fullTypeName,
      "hasTypeParameters" -> false.asInstanceOf[java.lang.Boolean]
    ) ++ (if (typeParameters.nonEmpty)
            Map(
              "hasTypeParameters" -> true.asInstanceOf[java.lang.Boolean],
              "typeParameter"     -> typeParameters.map(_.asMap).head,
              "typeParameters"    -> typeParameters.map(_.asMap).asJava
            )
          else Map.empty)).asJava
  override def asScalaDesc: OtherTypeDesc =
    copy(typeParameters = typeParameters.map(_.asScalaDesc))
}

case class CompletableFutureDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def packageName: Option[String] = Some("java.util.concurrent")
  override def simpleTypeName: String      = s"CompletableFuture"
  override def fullTypeName: String        = s"CompletableFuture[${valueTypeName.asString}]"
  override def asString: String            = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName,
                        "fullTypeName"   -> fullTypeName,
                        "valueTypeName"  -> valueTypeName.simpleTypeName).asJava
  def asScalaDesc: ScalaFutureDesc = ScalaFutureDesc(valueTypeName.asScalaDesc)
}

case class ScalaFutureDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def packageName: Option[String] = Some("scala.concurrent")
  override def simpleTypeName: String      = s"Future"
  override def fullTypeName: String        = s"Future[${valueTypeName.asString}]"
  override def asString: String            = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName,
                        "fullTypeName"   -> fullTypeName,
                        "valueTypeName"  -> valueTypeName.simpleTypeName).asJava
  override def asScalaDesc: ScalaFutureDesc = this
}

case class SeqTypeDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def packageName: Option[String] = Some("scala.collection")
  override def simpleTypeName: String      = "Seq"
  override def fullTypeName: String        = s"Seq[${valueTypeName.asString}]"
  override def asString: String            = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName,
                        "fullTypeName"   -> fullTypeName,
                        "valueTypeName"  -> valueTypeName.simpleTypeName).asJava
  override def asScalaDesc: SeqTypeDesc = this
}

case class JavaListTypeDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def packageName: Option[String] = Some("java.util")
  override def simpleTypeName: String      = "List"
  override def fullTypeName: String        = s"List[${valueTypeName.asString}]"
  override def asString: String            = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName,
                        "fullTypeName"   -> fullTypeName,
                        "valueTypeName"  -> valueTypeName.simpleTypeName).asJava
  override def asScalaDesc = SeqTypeDesc(valueTypeName.asScalaDesc)
}

case class ArrayTypeDesc(valueTypeName: TypeDesc) extends TypeDesc {
  override def packageName: Option[String] = Some("scala")
  override def simpleTypeName: String      = "Array"
  override def fullTypeName: String        = s"Array[${valueTypeName.asString}]"
  override def asString: String            = fullTypeName
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("simpleTypeName" -> simpleTypeName,
                        "fullTypeName"   -> fullTypeName,
                        "valueTypeName"  -> valueTypeName.simpleTypeName).asJava
  override def asScalaDesc: ArrayTypeDesc = copy(valueTypeName = valueTypeName.asScalaDesc)
}

case class ParameterTypeDesc(name: String, typeName: TypeDesc, notNull: Boolean) extends TypeDesc {
  override def packageName: Option[String] = None
  override def simpleTypeName: String      = typeName.simpleTypeName
  override def fullTypeName: String        = typeName.fullTypeName
  override def asString: String            = s"$name: ${typeName.asString}"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("name"     -> name,
                        "typeName" -> typeName.asMap,
                        "notNull"  -> notNull.asInstanceOf[java.lang.Boolean]).asJava
  override def asScalaDesc: ParameterTypeDesc = copy(typeName = typeName.asScalaDesc)
}

case class ConstructorDesc(parameters: Seq[ParameterTypeDesc]) extends TypeDesc {
  override def packageName: Option[String] = None
  override def simpleTypeName: String      = "Constructor"
  override def fullTypeName: String        = simpleTypeName
  override def asString: String =
    "(" + parameters.map(_.asString).mkString(",") + ")"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef]("parameters" -> parameters.map(_.asMap).asJava).asJava
  override def asScalaDesc: ConstructorDesc = copy(parameters = parameters.map(_.asScalaDesc))
}

case class MethodDesc(name: String,
                      parameters: Seq[ParameterTypeDesc],
                      returnType: TypeDesc,
                      notNull: Boolean,
                      throws: Boolean,
                      static: Boolean)
    extends TypeDesc {
  override def packageName: Option[String] = None
  override def simpleTypeName: String      = "Method"
  override def fullTypeName: String        = simpleTypeName
  override def asString: String =
    s"def $name(${parameters.map(_.asString).mkString(",")}): ${returnType.asString}"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef](
      "name"       -> name,
      "parameters" -> parameters.map(_.asMap).asJava,
      "returnType" -> returnType.asMap,
      "notNull"    -> notNull.asInstanceOf[java.lang.Boolean],
      "static"     -> static.asInstanceOf[java.lang.Boolean]
    ).asJava
  override def asScalaDesc: MethodDesc =
    copy(parameters = parameters.map(_.asScalaDesc), returnType = returnType.asScalaDesc)
}

case class FieldDesc(name: String, fieldType: TypeDesc, notNull: Boolean, static: Boolean) extends TypeDesc {
  override def packageName: Option[String] = None
  override def simpleTypeName: String      = "Field"
  override def fullTypeName: String        = simpleTypeName
  override def asString: String            = s"field $name, $fieldType"
  override def asMap: util.Map[String, AnyRef] =
    Map[String, AnyRef](
      "name"      -> name,
      "fieldType" -> fieldType.asMap,
      "notNull"   -> notNull.asInstanceOf[java.lang.Boolean],
      "static"    -> static.asInstanceOf[java.lang.Boolean]
    ).asJava
  override def asScalaDesc: FieldDesc = copy(fieldType = fieldType.asScalaDesc)
}

case class EnumDesc(simpleTypeName: String, entries: Map[String, String], packageName: Option[String])
    extends TypeDesc {
  override def fullTypeName: String = simpleTypeName
  override def asString: String     = s"enum $simpleTypeName { $entries }"
  override def asMap: util.Map[String, AnyRef] =
    (Map[String, AnyRef]("simpleTypeName" -> simpleTypeName,
                         "fullTypeName"   -> fullTypeName,
                         "entries"        -> entries.asJava) ++
    (if (packageName.nonEmpty) Map("packageName" -> packageName.get) else Map.empty)).asJava
  override def asScalaDesc: EnumDesc = this
}

case class ClassDesc(simpleTypeName: String,
                     constructor: Option[ConstructorDesc],
                     methods: Seq[MethodDesc],
                     fields: Seq[FieldDesc],
                     path: Path,
                     isAbstract: Boolean,
                     isStatic: Boolean,
                     packageName: Option[String] = None)
    extends TypeDesc {
  override def fullTypeName: String = simpleTypeName
  override def asString: String = {
    s"class ${simpleTypeName}${constructor.map(_.asString).getOrElse("")} {" +
    methods.map(_.asString).mkString("\n\n  ", "\n\n  ", "\n\n") +
    "}"
  }
  override def asMap: util.Map[String, AnyRef] =
    (Map[String, AnyRef](
      "simpleTypeName" -> simpleTypeName,
      "fullTypeName"   -> fullTypeName,
      "abstract"       -> isAbstract.asInstanceOf[java.lang.Boolean],
      "static"         -> isStatic.asInstanceOf[java.lang.Boolean],
      "methods"        -> methods.map(_.asMap).asJava,
      "fields"         -> fields.map(_.asMap).asJava
    ) ++
    (if (packageName.nonEmpty) Map("packageName" -> packageName.get) else Map.empty)).asJava
  override def asScalaDesc: ClassDesc =
    copy(
      constructor = constructor.map(_.asScalaDesc),
      methods = methods.map(_.asScalaDesc),
      fields = fields.map(_.asScalaDesc)
    )
}
