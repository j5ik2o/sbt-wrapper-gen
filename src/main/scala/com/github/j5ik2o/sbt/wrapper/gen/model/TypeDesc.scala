package com.github.j5ik2o.sbt.wrapper.gen.model

import java.nio.file.Path
import java.util

import scala.collection.JavaConverters._

sealed trait TypeDesc {
  type ThisScalaType <: TypeDesc

  def simpleTypeName: String

  def asString: String = fullTypeName

  def asJavaMap: util.Map[String, AnyRef] =
    asScalaMap.asJava

  def asScalaMap: Map[String, AnyRef] =
    Map[String, AnyRef](
      "asString"       -> asString,
      "object"         -> isObject.asInstanceOf[java.lang.Boolean],
      "enum"           -> isEnum.asInstanceOf[java.lang.Boolean],
      "typeLevelCount" -> typeLevelCount.asInstanceOf[java.lang.Integer],
      "simpleTypeName" -> simpleTypeName,
      "fullTypeName"   -> fullTypeName,
      "hasPackageName" -> packageName.nonEmpty.asInstanceOf[java.lang.Boolean]
    ) ++ (if (packageName.nonEmpty) Map("packageName" -> packageName.get) else Map.empty)

  def typeLevelCount: Int = 1

  def isEnum: Boolean = false

  def isObject: Boolean = true

  def packageName: Option[String] = None

  def fullTypeName: String = simpleTypeName

  def asScalaDesc: ThisScalaType = this.asInstanceOf[ThisScalaType]

  protected def bottomValueTypeDesc(valueTypeDesc: TypeDesc): TypeDesc = {
    def loop(v: TypeDesc): TypeDesc = v match {
      case MapTypeDesc(_, n)     => loop(n)
      case SeqTypeDesc(n)        => loop(n)
      case ArrayTypeDesc(n)      => loop(n)
      case JavaListTypeDesc(n)   => loop(n)
      case JavaMapTypeDesc(_, n) => loop(n)
      case a                     => a
    }

    loop(valueTypeDesc)
  }
}

case class PrimitiveTypeDesc(primitiveType: PrimitiveType) extends TypeDesc {
  override lazy val simpleTypeName: String = primitiveType.entryName
}

case class UnitTypeDesc() extends TypeDesc {
  override lazy val packageName: Option[String] = Some("scala")
  override lazy val simpleTypeName: String      = "Unit"
}

case class VoidTypeDesc() extends TypeDesc {
  override type ThisScalaType = UnitTypeDesc
  override lazy val isObject: Boolean           = false
  override lazy val packageName: Option[String] = None
  override lazy val simpleTypeName: String      = "void"
  override lazy val asScalaDesc: UnitTypeDesc   = UnitTypeDesc()
}

case class StringTypeDesc() extends TypeDesc {
  override lazy val packageName: Option[String] = Some("java.lang")
  override lazy val simpleTypeName: String      = "String"
}

case class ArrayTypeDesc(valueTypeDesc: TypeDesc) extends TypeDesc {
  override type ThisScalaType = ArrayTypeDesc
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("scala")
  override lazy val simpleTypeName: String      = "Array"
  override lazy val fullTypeName: String        = s"Array[${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] = super.asScalaMap ++ Map[String, AnyRef](
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap
  )
  override lazy val asScalaDesc: ArrayTypeDesc = copy(valueTypeDesc = valueTypeDesc.asScalaDesc)

}

case class SeqTypeDesc(valueTypeDesc: TypeDesc) extends TypeDesc {
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("scala.collection")
  override lazy val simpleTypeName: String      = "Seq"
  override lazy val fullTypeName: String        = s"Seq[${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] = super.asScalaMap ++ Map[String, AnyRef](
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap
  )

}

case class JavaListTypeDesc(valueTypeDesc: TypeDesc) extends TypeDesc {
  override type ThisScalaType = SeqTypeDesc
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("java.util")
  override lazy val simpleTypeName: String      = "List"
  override lazy val fullTypeName: String        = s"List[${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] = super.asScalaMap ++ Map[String, AnyRef](
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap
  )
  override lazy val asScalaDesc: SeqTypeDesc = SeqTypeDesc(valueTypeDesc.asScalaDesc)

}

case class MapTypeDesc(keyTypeDesc: TypeDesc, valueTypeDesc: TypeDesc) extends TypeDesc {
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("scala.collection")
  override lazy val simpleTypeName: String      = "Map"
  override lazy val fullTypeName: String        = s"Map[${keyTypeDesc.asString}, ${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] = super.asScalaMap ++ Map[String, AnyRef](
    "keyTypeDesc"         -> keyTypeDesc.asJavaMap,
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap
  )
}

case class JavaMapTypeDesc(keyTypeDesc: TypeDesc, valueTypeDesc: TypeDesc) extends TypeDesc {
  override type ThisScalaType = MapTypeDesc
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("java.util")
  override lazy val simpleTypeName: String      = "Map"
  override lazy val fullTypeName: String        = s"Map[${keyTypeDesc.asString}, ${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] = super.asScalaMap ++ Map[String, AnyRef](
    "keyTypeDesc"         -> keyTypeDesc.asJavaMap,
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap
  )
  override lazy val asScalaDesc: MapTypeDesc = MapTypeDesc(keyTypeDesc.asScalaDesc, valueTypeDesc.asScalaDesc)

}

case class WildcardTypeDesc(typeName: String) extends TypeDesc {
  override lazy val typeLevelCount: Int                 = 1
  override lazy val packageName: Option[String]         = None
  override lazy val fullTypeName: String                = simpleTypeName
  override lazy val asString: String                    = simpleTypeName
  override lazy val asJavaMap: util.Map[String, AnyRef] = Map[String, AnyRef]("wildcard" -> simpleTypeName).asJava
  override lazy val simpleTypeName: String              = typeName
}

case class OtherTypeDesc(typeName: String, typeParameterDescs: Seq[TypeDesc], override val packageName: Option[String])
    extends TypeDesc {
  require(typeName != null, "typeName is null")
  require(!typeParameterDescs.contains(null), "typeParameterDescs contains null")
  require(packageName.fold(true)(_ != null), s"packageName value is null: $typeName")
  override type ThisScalaType = OtherTypeDesc
  override lazy val typeLevelCount: Int = 1 + typeParameterDescs.map(_.typeLevelCount).sum
  override lazy val fullTypeName: String =
  simpleTypeName + (if (typeParameterDescs.nonEmpty) typeParameterDescs.map(_.simpleTypeName).mkString("[", ",", "]")
                    else "")
  override lazy val simpleTypeName: String = typeName
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "hasTypeParameters" -> typeParameterDescs.nonEmpty.asInstanceOf[java.lang.Boolean]
  ) ++ (if (typeParameterDescs.nonEmpty)
          Map(
            "hasTypeParameters"  -> true.asInstanceOf[java.lang.Boolean],
            "typeParameterDesc"  -> typeParameterDescs.map(_.asJavaMap).head,
            "typeParameterDescs" -> typeParameterDescs.map(_.asJavaMap).asJava
          )
        else Map.empty)

  override lazy val asScalaDesc: OtherTypeDesc =
    copy(typeParameterDescs = typeParameterDescs.map(_.asScalaDesc))
}

case class JavaFutureDesc(valueTypeDesc: TypeDesc) extends TypeDesc {
  override type ThisScalaType = ScalaFutureDesc
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("java.util.concurrent")
  override lazy val simpleTypeName: String      = s"Future"
  override lazy val fullTypeName: String        = s"Future[${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap
  )
  override lazy val asScalaDesc: ScalaFutureDesc = ScalaFutureDesc(valueTypeDesc.asScalaDesc)

}

case class CompletableFutureDesc(valueTypeDesc: TypeDesc) extends TypeDesc {
  override type ThisScalaType = ScalaFutureDesc
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("java.util.concurrent")
  override lazy val simpleTypeName: String      = s"CompletableFuture"
  override lazy val fullTypeName: String        = s"CompletableFuture[${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap
  )
  override lazy val asScalaDesc: ScalaFutureDesc = ScalaFutureDesc(valueTypeDesc.asScalaDesc)

}

case class ScalaFutureDesc(valueTypeDesc: TypeDesc) extends TypeDesc {
  override lazy val typeLevelCount: Int         = 1 + valueTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = Some("scala.concurrent")
  override lazy val simpleTypeName: String      = s"Future"
  override lazy val fullTypeName: String        = s"Future[${valueTypeDesc.asString}]"
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "valueTypeDesc"       -> valueTypeDesc.asJavaMap,
    "bottomValueTypeDesc" -> bottomValueTypeDesc(valueTypeDesc).asJavaMap,
  )

}

case class ParameterTypeDesc(name: String, parameterTypeDesc: TypeDesc, notNull: Boolean) extends TypeDesc {
  override type ThisScalaType = ParameterTypeDesc
  override lazy val typeLevelCount: Int         = 0 + parameterTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = None
  override lazy val simpleTypeName: String      = parameterTypeDesc.simpleTypeName
  override lazy val fullTypeName: String        = parameterTypeDesc.fullTypeName
  override lazy val asString: String            = s"$name: ${parameterTypeDesc.asString}"
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "name"                    -> name,
    "parameterTypeDesc"       -> parameterTypeDesc.asJavaMap,
    "bottomParameterTypeDesc" -> bottomValueTypeDesc(parameterTypeDesc).asJavaMap,
    "notNull"                 -> notNull.asInstanceOf[java.lang.Boolean]
  )
  override lazy val asScalaDesc: ParameterTypeDesc = copy(parameterTypeDesc = parameterTypeDesc.asScalaDesc)

}

case class ConstructorDesc(parameterTypeDescs: Seq[ParameterTypeDesc]) extends TypeDesc {
  override type ThisScalaType = ConstructorDesc
  override lazy val typeLevelCount: Int         = 0 + parameterTypeDescs.map(_.typeLevelCount).sum
  override lazy val packageName: Option[String] = None
  override lazy val fullTypeName: String        = simpleTypeName
  override lazy val simpleTypeName: String      = "Constructor"
  override lazy val asString: String =
  "(" + parameterTypeDescs.map(_.asString).mkString(",") + ")"
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "parameterTypeDescs" -> parameterTypeDescs.map(_.asJavaMap).asJava
  )
  override lazy val asScalaDesc: ConstructorDesc = copy(parameterTypeDescs = parameterTypeDescs.map(_.asScalaDesc))
}

case class MethodDesc(name: String,
                      parameterTypeDescs: Seq[ParameterTypeDesc],
                      returnTypeDesc: TypeDesc,
                      notNull: Boolean,
                      throws: Boolean,
                      static: Boolean)
    extends TypeDesc {
  override type ThisScalaType = MethodDesc
  override lazy val typeLevelCount: Int         = 0 + parameterTypeDescs.map(_.typeLevelCount).sum
  override lazy val packageName: Option[String] = None
  override lazy val fullTypeName: String        = simpleTypeName
  override lazy val simpleTypeName: String      = "Method"
  override lazy val asString: String =
    s"def $name(${parameterTypeDescs.map(_.asString).mkString(",")}): ${returnTypeDesc.asString}"

  lazy val isGetter: Boolean        = name.startsWith("get")
  lazy val isSetter: Boolean        = name.startsWith("set")
  lazy val isBooleanGetter: Boolean = name.startsWith("is")

  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "name"                    -> name,
    "parameterTypeDescs"      -> parameterTypeDescs.map(_.asJavaMap).asJava,
    "bottomParameterTypeDesc" -> parameterTypeDescs.map(v => bottomValueTypeDesc(v).asJavaMap).asJava,
    "returnTypeDesc"          -> returnTypeDesc.asJavaMap,
    "bottomReturnTypeDesc"    -> bottomValueTypeDesc(returnTypeDesc).asJavaMap,
    "notNull"                 -> notNull.asInstanceOf[java.lang.Boolean],
    "throws"                  -> throws.asInstanceOf[java.lang.Boolean],
    "static"                  -> static.asInstanceOf[java.lang.Boolean],
    "isGetter"                -> isGetter.asInstanceOf[java.lang.Boolean],
    "isSetter"                -> isSetter.asInstanceOf[java.lang.Boolean],
    "isBooleanGetter"         -> isBooleanGetter.asInstanceOf[java.lang.Boolean]
  )

  override lazy val asScalaDesc: MethodDesc =
    copy(parameterTypeDescs = parameterTypeDescs.map(_.asScalaDesc), returnTypeDesc = returnTypeDesc.asScalaDesc)
}

case class FieldDesc(name: String, fieldTypeDesc: TypeDesc, notNull: Boolean, static: Boolean) extends TypeDesc {
  override type ThisScalaType = FieldDesc
  override lazy val typeLevelCount: Int         = 0 + fieldTypeDesc.typeLevelCount
  override lazy val packageName: Option[String] = None
  override lazy val fullTypeName: String        = simpleTypeName
  override lazy val simpleTypeName: String      = "Field"
  override lazy val asString: String            = s"field $name, $fieldTypeDesc"
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "name"                -> name,
    "fieldTypeDesc"       -> fieldTypeDesc.asJavaMap,
    "bottomFieldTypeDesc" -> bottomValueTypeDesc(fieldTypeDesc).asJavaMap,
    "notNull"             -> notNull.asInstanceOf[java.lang.Boolean],
    "static"              -> static.asInstanceOf[java.lang.Boolean]
  )
  override lazy val asScalaDesc: FieldDesc = copy(fieldTypeDesc = fieldTypeDesc.asScalaDesc)
}

case class EnumDesc(simpleTypeName: String, entries: Map[String, Seq[String]], override val packageName: Option[String])
    extends TypeDesc {
  override type ThisScalaType = EnumDesc
  override lazy val isEnum: Boolean      = true
  override lazy val typeLevelCount: Int  = 1
  override lazy val fullTypeName: String = simpleTypeName
  override lazy val asString: String     = s"enum $simpleTypeName { $entries }"
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "entries" -> entries.mapValues(_.asJava).asJava
  )
  override lazy val asScalaDesc: EnumDesc = this
}

case class ClassDesc(simpleTypeName: String,
                     constructor: Option[ConstructorDesc],
                     methods: Seq[MethodDesc],
                     fields: Seq[FieldDesc],
                     isAbstract: Boolean,
                     isStatic: Boolean,
                     classJavadocText: Option[String],
                     path: Path,
                     override val packageName: Option[String] = None)
    extends TypeDesc {
  override type ThisScalaType = ClassDesc
  override lazy val typeLevelCount: Int  = 1
  override lazy val fullTypeName: String = simpleTypeName
  override lazy val asString: String = {
    s"class ${simpleTypeName}${constructor.map(_.asString).getOrElse("")} {" +
    methods.map(_.asString).mkString("\n\n  ", "\n\n  ", "\n\n") +
    "}"
  }
  override lazy val asScalaMap: Map[String, AnyRef] =
  super.asScalaMap ++ Map[String, AnyRef](
    "abstract" -> isAbstract.asInstanceOf[java.lang.Boolean],
    "static"   -> isStatic.asInstanceOf[java.lang.Boolean],
    "methods"  -> methods.map(_.asJavaMap).asJava,
    "fields"   -> fields.map(_.asJavaMap).asJava,
    "path"     -> path.toString,
    "javadoc"  -> classJavadocText
  )
  override lazy val asScalaDesc: ClassDesc =
    copy(
      constructor = constructor.map(_.asScalaDesc),
      methods = methods.map(_.asScalaDesc),
      fields = fields.map(_.asScalaDesc)
    )
}
