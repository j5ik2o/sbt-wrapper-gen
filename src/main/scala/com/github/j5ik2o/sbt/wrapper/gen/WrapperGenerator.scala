package com.github.j5ik2o.sbt.wrapper.gen

import java.io.FileWriter
import java.nio.file.{ Files, Path }
import java.util.concurrent.atomic.AtomicBoolean

import com.github.j5ik2o.sbt.wrapper.gen.model._
import com.github.j5ik2o.sbt.wrapper.gen.util.Loan._
import com.github.javaparser.ast.Modifier.Keyword
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments.JavadocComment
import com.github.javaparser.ast.visitor.{ GenericVisitorAdapter, VoidVisitorAdapter }
import com.github.javaparser.ast.{ Modifier, PackageDeclaration }
import com.github.javaparser.{ JavaParser, ParserConfiguration }
import freemarker.cache.{ FileTemplateLoader, MultiTemplateLoader, TemplateLoader }
import sbt.Keys._
import sbt.complete.Parser
import sbt.{ Def, File, _ }

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.compat.java8.OptionConverters
import scala.compat.java8.StreamConverters._
import scala.util.{ Success, Try }

object WrapperGenerator {
  type TypeFilter            = TypeDesc => Boolean
  type TypeDescMapper        = (String, Seq[TypeDesc]) => TypeDesc
  type TemplateNameMapper    = (String, TypeDesc) => String
  type OutputDirectoryMapper = TypeDesc => File
  type TypeNameMapper        = TypeDesc => Seq[(String, String)]
  type PackageNameMapper     = (String, String, TypeDesc) => String

  val defaultTypeDescMapper: TypeDescMapper = {
    case ("int", _) =>
      PrimitiveTypeDesc(PrimitiveType.INT)
    case ("Integer", _) =>
      PrimitiveTypeDesc(PrimitiveType.INT)
    case ("long", _) =>
      PrimitiveTypeDesc(PrimitiveType.LONG)
    case ("Long", _) =>
      PrimitiveTypeDesc(PrimitiveType.LONG)
    case ("short", _) =>
      PrimitiveTypeDesc(PrimitiveType.SHORT)
    case ("Short", _) =>
      PrimitiveTypeDesc(PrimitiveType.SHORT)
    case ("boolean", _) =>
      PrimitiveTypeDesc(PrimitiveType.BOOLEAN)
    case ("Boolean", _) =>
      PrimitiveTypeDesc(PrimitiveType.BOOLEAN)
    case ("byte", _) =>
      PrimitiveTypeDesc(PrimitiveType.BYTE)
    case ("Byte", _) =>
      PrimitiveTypeDesc(PrimitiveType.BYTE)
    case ("char", _) =>
      PrimitiveTypeDesc(PrimitiveType.CHAR)
    case ("Char", _) =>
      PrimitiveTypeDesc(PrimitiveType.CHAR)
    case ("double", _) =>
      PrimitiveTypeDesc(PrimitiveType.DOUBLE)
    case ("Double", _) =>
      PrimitiveTypeDesc(PrimitiveType.DOUBLE)
    case ("float", _) =>
      PrimitiveTypeDesc(PrimitiveType.FLOAT)
    case ("Float", _) =>
      PrimitiveTypeDesc(PrimitiveType.FLOAT)
    case ("String", _) =>
      StringTypeDesc()
    case ("void", _) =>
      VoidTypeDesc()
    case ("Void", _) =>
      VoidTypeDesc()
    case ("Array", Seq(valueType)) =>
      ArrayTypeDesc(valueType)
    case ("List", Seq(valueType)) =>
      JavaListTypeDesc(valueType)
    case ("Map", Seq(keyType, valueType)) =>
      JavaMapTypeDesc(keyType, valueType)
    case ("CompletableFuture", Seq(paramType)) =>
      CompletableFutureDesc(paramType)
    case (other, paramTypes) =>
      OtherTypeDesc(other, paramTypes, None)
  }
}

trait WrapperGenerator {

  import SbtWrapperGenKeys._
  import WrapperGenerator._

  type RESULT = GeneratorContext

  import complete.DefaultParsers._

  private val oneStringParser: Parser[String] = token(Space ~> StringBasic, "java class name")

  private val manyStringParser: Parser[Seq[String]] = token(Space ~> StringBasic, "java class name") +

  def parseType(context: GeneratorContext, t: Type): TypeDesc = {
    val typeMapper = context.typeDescMapper
    t match {
      case t if t.isArrayType =>
        val at        = t.asArrayType()
        val paramType = parseType(context, at.getElementType)
        typeMapper("Array", Seq(paramType))
      case t if t.isClassOrInterfaceType =>
        val cit      = t.asClassOrInterfaceType()
        val typeName = cit.getName.getIdentifier
        val ta = OptionConverters
          .toScala(cit.getTypeArguments).map { typeArguments =>
            typeArguments.asScala.flatMap { t =>
              val result = parseType(context, t)
              if (result == null)
                Seq.empty
              else
                Seq(result)
            }
          }.getOrElse(Seq.empty)
        typeMapper(typeName, ta)
      case t if t.isIntersectionType =>
        val it = t.asIntersectionType()
        context.logger.debug(s"it = $it")
        null
      case t if t.isPrimitiveType =>
        val pt       = t.asPrimitiveType()
        val typeName = pt.asString
        typeMapper(typeName, Seq.empty)
      case t if t.isReferenceType =>
        val rt = t.asReferenceType()
        context.logger.debug(s"rt = $rt")
        null
      case t if t.isTypeParameter =>
        val tp = t.asTypeParameter()
        context.logger.debug(s"tp = $tp")
        null
      case t if t.isUnionType =>
        val ut = t.asUnionType()
        context.logger.debug(s"ut1 = $ut")
        null
      case t if t.isUnknownType =>
        val ut = t.asUnionType()
        context.logger.debug(s"ut2 = $ut")
        null
      case t if t.isVarType =>
        val vt = t.asVarType()
        context.logger.debug(s"vt = $vt")
        null
      case t if t.isVoidType => VoidTypeDesc()
      case t if t.isWildcardType =>
        val wt = t.asWildcardType()
        context.logger.debug(s"wt = $wt")
        OptionConverters.toScala(wt.getExtendedType).foreach { rt =>
          context.logger.debug("wt.extended" + rt.toString)
        }
        null
    }
  }

  def generateOneTask: Def.Initialize[InputTask[Seq[File]]] = Def.inputTask {
    val className       = oneStringParser.parsed
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateOne task")

    val typeFilterValue            = (typeDescFilter in scalaWrapperGen).value
    val typeDescMapperValue        = (typeDescMapper in scalaWrapperGen).value
    val templateDirectoryValue     = (templateDirectories in scalaWrapperGen).value
    val templateNameMapperValue    = (templateNameMapper in scalaWrapperGen).value
    val inputDirectoryValue        = (inputSourceDirectory in scalaWrapperGen).value
    val outputDirectoryMapperValue = (outputSourceDirectoryMapper in scalaWrapperGen).value
    val typeNameMapperValue        = (typeNameMapper in scalaWrapperGen).value
    val packageNameMapperValue     = (packageNameMapper in scalaWrapperGen).value
    val parserConfigurationValue   = (javaParserConfiguration in scalaWrapperGen).value

    val ctx: GeneratorContext = GeneratorContext(
      logger = streams.value.log,
      typeFilter = typeFilterValue,
      typeDescMapper = typeDescMapperValue,
      templateDirectories = templateDirectoryValue,
      templateNameMapper = templateNameMapperValue,
      inputDirectory = inputDirectoryValue,
      outputDirectoryMapper = outputDirectoryMapperValue,
      typeNameMapper = typeNameMapperValue,
      packageNameMapper = packageNameMapperValue,
      parserConfigurationOpt = parserConfigurationValue
    )
    _generateOne(ctx, className).get
  }

  private[gen] def _generateOne(context: GeneratorContext, className: String): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateOne: start")
    val result: Try[Seq[File]] = for {
      cfg       <- createTemplateConfiguration(context.templateDirectories)
      typeDescs <- getTypeDescs(context)(context.typeFilter)
      files <- typeDescs
        .find { v =>
          containTypeName(className, v)
        }
        .map { classDesc =>
          generateFiles(context, cfg, classDesc)
        }
        .get
    } yield files
    logger.debug(s"generateOne: finished = $result")
    result
  }

  def generateManyTask: Def.Initialize[InputTask[Seq[File]]] = Def.inputTask {
    val typeNames       = manyStringParser.parsed
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateOne task")

    val typeFilterValue            = (typeDescFilter in scalaWrapperGen).value
    val typeDescMapperValue        = (typeDescMapper in scalaWrapperGen).value
    val templateDirectoriesValue   = (templateDirectories in scalaWrapperGen).value
    val templateNameMapperValue    = (templateNameMapper in scalaWrapperGen).value
    val inputDirectoryValue        = (inputSourceDirectory in scalaWrapperGen).value
    val outputDirectoryMapperValue = (outputSourceDirectoryMapper in scalaWrapperGen).value
    val typeNameMapperValue        = (typeNameMapper in scalaWrapperGen).value
    val packageNameMapperValue     = (packageNameMapper in scalaWrapperGen).value
    val parserConfigurationValue   = (javaParserConfiguration in scalaWrapperGen).value

    val context: GeneratorContext = GeneratorContext(
      logger = streams.value.log,
      typeFilter = typeFilterValue,
      typeDescMapper = typeDescMapperValue,
      templateDirectories = templateDirectoriesValue,
      templateNameMapper = templateNameMapperValue,
      inputDirectory = inputDirectoryValue,
      outputDirectoryMapper = outputDirectoryMapperValue,
      typeNameMapper = typeNameMapperValue,
      packageNameMapper = packageNameMapperValue,
      parserConfigurationOpt = parserConfigurationValue
    )
    _generateMany(context, typeNames).get
  }

  private[gen] def _generateMany(context: GeneratorContext, classNames: Seq[String]): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateMany($classNames): start")
    val result = for {
      cfg       <- createTemplateConfiguration(context.templateDirectories)
      typeDescs <- getTypeDescs(context)(context.typeFilter)
      files <- typeDescs
        .filter { classDesc =>
          classNames.exists { cn =>
            containTypeName(cn, classDesc)
          }
        }
        .foldLeft(Try(Seq.empty[File])) { (result, classDesc) =>
          for {
            r1 <- result
            r2 <- generateFiles(context, cfg, classDesc)
          } yield r1 ++ r2
        }
    } yield files
    logger.debug(s"generateMany: finished = $result")
    result
  }

  private def containTypeName(className: String, typeDesc: TypeDesc): Boolean = {
    typeDesc.simpleTypeName == className || typeDesc.packageName.fold[String](typeDesc.simpleTypeName) { p =>
      s"$p.${typeDesc.simpleTypeName}"
    } == className
  }

//  private[gen] def getTypeDescs(context: GeneratorContext)(typeFilter: TypeFilter = { _: TypeDesc =>
//    true
//  }): Try[Seq[TypeDesc]] = {
//    _getTypeDescs(context)().flatMap { result =>
//      _getTypeDescs(context.copy(allTypeDesc = result.toSet))(typeFilter)
//    }
//  }

  private[gen] def getTypeDescs(context: GeneratorContext)(typeFilter: TypeFilter = { _: TypeDesc =>
    true
  }): Try[Seq[TypeDesc]] = {
    Try {
      context.logger.debug(s"getTypeDescs($context): start")
      def loop(path: Path): Seq[TypeDesc] = {
        val files = Files.list(path).toScala
        //context.logger.debug(s"files = $files")
        val r = files.filterNot(_.endsWith("package-info.java")).flatMap { path =>
          if (path.toFile.isDirectory) {
            loop(path)
          } else {
            context.logger.debug(s"path = $path")
            val compileUnit = context.javaParser.parse(path)
            val cpResult    = compileUnit.getResult
            // context.logger.debug(s"cpResult = $cpResult")
            val resultOpt = OptionConverters.toScala(cpResult)
            resultOpt
              .map { result =>
                val visitor = new Visitor()
                result.accept(visitor, context)
                val typeDesc = visitor.result(path)
                // context.logger.debug(s"typeDesc: ${typeDesc.simpleTypeName}, ${typeDesc}")
                Seq(typeDesc).filter(typeFilter)
              }.getOrElse(Seq.empty)
          }
        }
        r
      }

      val r = loop(context.inputDirectory.toPath)
      context.logger.debug(s"getTypeDescs: finished = $r")
      r
    }
  }

  private[gen] def createTemplateConfiguration(
      templateDirectories: Seq[File]
  )(implicit logger: Logger): Try[freemarker.template.Configuration] = Try {
    logger.debug(s"createTemplateConfiguration($templateDirectories): start")
    var cfg: freemarker.template.Configuration = null
    try {
      cfg = new freemarker.template.Configuration(
        freemarker.template.Configuration.DEFAULT_INCOMPATIBLE_IMPROVEMENTS
      )
      val loaders = templateDirectories
        .map { f =>
          new FileTemplateLoader(f)
        }.toArray[TemplateLoader]
      val templateLoader = new MultiTemplateLoader(loaders)
      cfg.setTemplateLoader(templateLoader)
    } finally {
      logger.debug(s"createTemplateConfiguration: finished = $cfg")
    }
    cfg
  }

  private[gen] def generateFiles(context: GeneratorContext,
                                 cfg: freemarker.template.Configuration,
                                 typeDesc: TypeDesc): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateFiles($cfg, $typeDesc): start")
    val outputTargetDirectory  = context.outputDirectoryMapper(typeDesc)
    val typeNameWithExtensions = context.typeNameMapper(typeDesc)
    val result =
      typeNameWithExtensions.foldLeft(Try(Seq.empty[File])) {
        case (result, (typeName, extension)) =>
          for {
            r <- result
            e <- generateFile(context, cfg, typeDesc, typeName, extension, outputTargetDirectory)
          } yield r :+ e
      }
    logger.debug(s"generateFiles: finished = $result")
    result
  }

  private[gen] def generateFile(context: GeneratorContext,
                                cfg: freemarker.template.Configuration,
                                typeDesc: TypeDesc,
                                typeName: String,
                                extension: String,
                                outputDirectory: File): Try[File] = {
    implicit val logger = context.logger
    logger.debug(s"generateFile($cfg, $typeDesc, $typeName, $outputDirectory): start")
    val templateName = context.templateNameMapper(typeName, typeDesc)
    val template     = cfg.getTemplate(templateName)

    val ou = outputDirectory / typeDesc.packageName
      .map(p => context.packageNameMapper(p, typeName, typeDesc)).map(_.replace(".", "/")).getOrElse("")
    context.logger.debug(s"ou = $ou")
    val file = createFile(ou, typeName, extension)
    context.logger.debug(
      s"classDesc = $typeDesc, className = $typeName, templateName = $templateName, generate file = $file"
    )

    if (!ou.exists())
      IO.createDirectory(ou)

    val result = using(new FileWriter(file)) { writer =>
      logger.debug("typeDesc.asMap = " + typeDesc.asJavaMap)
      template.process(typeDesc.asScalaDesc.asJavaMap, writer)
      writer.flush()
      Success(file)
    }
    // logger.debug(FileUtil.readText(file))
    logger.debug(s"generateFile: finished = $result")
    result
  }

  private[gen] def createFile(outputDirectory: File, className: String, extension: String)(
      implicit logger: Logger
  ): File = {
    logger.debug(s"createFile($outputDirectory, $className): start")
    val file = outputDirectory / (className + extension)

    logger.debug(s"createFile: finished = $file")
    file
  }

  def generateAllTask: Def.Initialize[Task[Seq[File]]] = Def.task {
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateAll task")

    val typeDescToBoolean          = (typeDescFilter in scalaWrapperGen).value
    val typeDescMapperValue        = (typeDescMapper in scalaWrapperGen).value
    val templateDirectoriesValue   = (templateDirectories in scalaWrapperGen).value
    val templateNameMapperValue    = (templateNameMapper in scalaWrapperGen).value
    val inputDirectoryValue        = (inputSourceDirectory in scalaWrapperGen).value
    val outputDirectoryMapperValue = (outputSourceDirectoryMapper in scalaWrapperGen).value
    val typeNameMapperValue        = (typeNameMapper in scalaWrapperGen).value
    val packageNameMapperValue     = (packageNameMapper in scalaWrapperGen).value
    val parserConfigurationValue   = (javaParserConfiguration in scalaWrapperGen).value

    val context: GeneratorContext = GeneratorContext(
      logger = streams.value.log,
      typeFilter = typeDescToBoolean,
      typeDescMapper = typeDescMapperValue,
      templateDirectories = templateDirectoriesValue,
      templateNameMapper = templateNameMapperValue,
      inputDirectory = inputDirectoryValue,
      outputDirectoryMapper = outputDirectoryMapperValue,
      typeNameMapper = typeNameMapperValue,
      packageNameMapper = packageNameMapperValue,
      parserConfigurationOpt = parserConfigurationValue
    )
    _generateAll(context).get
  }

  private[gen] def _generateAll(context: GeneratorContext): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateAll: start")
    val result = for {
      cfg       <- createTemplateConfiguration(context.templateDirectories)
      typeDescs <- getTypeDescs(context)(context.typeFilter)
      files <- typeDescs
        .foldLeft(Try(Seq.empty[File])) { (result, classDesc) =>
          for {
            r1 <- result
            r2 <- generateFiles(context, cfg, classDesc)
          } yield r1 ++ r2
        }
    } yield files
    logger.debug(s"generateAll: finished = $result")
    result
  }

  case class GeneratorContext(logger: Logger,
                              typeFilter: TypeFilter,
                              typeDescMapper: TypeDescMapper,
                              templateDirectories: Seq[File],
                              templateNameMapper: TemplateNameMapper,
                              inputDirectory: File,
                              outputDirectoryMapper: OutputDirectoryMapper,
                              typeNameMapper: TypeNameMapper,
                              packageNameMapper: PackageNameMapper,
                              parserConfigurationOpt: Option[ParserConfiguration],
                              allTypeDesc: Set[TypeDesc] = Set.empty) {
    val javaParser = parserConfigurationOpt.map(pc => new JavaParser(pc)).getOrElse(new JavaParser())
  }

  class ClassVisitor extends GenericVisitorAdapter[String, RESULT] {
    override def visit(n: ClassOrInterfaceDeclaration, arg: RESULT): String = {
      super.visit(n, arg)
      n.getName.asString
    }
  }

  class PackageVisitor extends GenericVisitorAdapter[Option[String], RESULT] {
    override def visit(n: PackageDeclaration, arg: RESULT): Option[String] = {
      super.visit(n, arg)
      OptionConverters
        .toScala(n.getName.getQualifier).map { q =>
          q.asString() + "." + n.getName.getIdentifier
        }.orElse(Some(n.getName.getIdentifier))
    }
  }

  class EnumVisitor(exist: AtomicBoolean) extends VoidVisitorAdapter[RESULT] {

    override def visit(n: EnumDeclaration, arg: RESULT): Unit = {
      exist.set(true)
      super.visit(n, arg)
    }
  }

  class Visitor extends VoidVisitorAdapter[RESULT] {

    private val methodDescs                              = ArrayBuffer.empty[MethodDesc]
    private val fieldDescs                               = ArrayBuffer.empty[FieldDesc]
    private var typeName: String                         = _
    private var packageName: Option[String]              = None
    private var constructorDesc: Option[ConstructorDesc] = None
    private var enum: Boolean                            = false
    private var entries: Map[String, Seq[String]]        = Map.empty
    private var isStatic: Boolean                        = false
    private var isAbstract: Boolean                      = false
    private var classJavadocText: Option[String]         = None

    private var counter = 0L

    def result(path: Path): TypeDesc = {
      require(typeName != null, "typeName is null")
      val result = if (enum) {
        EnumDesc(typeName, entries, packageName)
      } else {
        ClassDesc(typeName,
                  constructorDesc,
                  methodDescs.result.toVector,
                  fieldDescs.result.toVector,
                  isAbstract,
                  isStatic,
                  classJavadocText,
                  path,
                  packageName)
      }
      result
    }

    override def visit(n: FieldDeclaration, arg: RESULT): Unit = {
      arg.logger.debug(s"counter = ${counter}, ${n}")
      counter += 1
      val currentTypeName = n.getParentNode.get.accept(new ClassVisitor(), arg)
      if (currentTypeName == typeName) {
        val isStatic = n.getModifiers.asScala.exists {
          _.getKeyword == Keyword.STATIC
        }
        if (n.getModifiers.asScala.exists {
              _.getKeyword == Keyword.PRIVATE
            }) {
          n.getVariables.asScala.foreach { v =>
            val n = v.getName.asString()
            val t = parseType(arg, v.getType)
            val notNull = v.getType.getAnnotations.asScala
              .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
            fieldDescs.append(FieldDesc(n, t, notNull, isStatic))
          }
        }
      }
      super.visit(n, arg)
    }

    override def visit(n: EnumDeclaration, arg: RESULT): Unit = {
      arg.logger.debug(s"counter = ${counter}, ${n}")
      counter += 1
      if (typeName == null) {
        enum = true
        typeName = n.getName.getIdentifier
        entries = n.getEntries.asScala.map { e =>
          (e.getName.asString(), e.getArguments.asScala.map(_.toString))
        }.toMap
      }
      super.visit(n, arg)
    }

    override def visit(n: PackageDeclaration, arg: RESULT): Unit = {
      arg.logger.debug(s"counter = ${counter}, ${n}")
      counter += 1
      if (packageName.isEmpty) {
        packageName = OptionConverters
          .toScala(n.getName.getQualifier).map { q =>
            q.asString() + "." + n.getName.getIdentifier
          }.orElse(Some(n.getName.getIdentifier))
      }
      super.visit(n, arg)
    }

    override def visit(n: ClassOrInterfaceDeclaration, arg: RESULT): Unit = {
      arg.logger.debug(s"counter = ${counter}, ${n}")
      counter += 1
      if (typeName == null) {
        isStatic = n.getModifiers.contains(Modifier.staticModifier())
        isAbstract = n.getModifiers.contains(Modifier.abstractModifier())
        val name = n.asClassOrInterfaceDeclaration().getName
        typeName = name.asString()
      }
      super.visit(n, arg)
    }

    override def visit(n: ConstructorDeclaration, arg: RESULT): Unit = {
      arg.logger.debug(s"counter = ${counter}, ${n}")
      counter += 1
      if (constructorDesc == None) {
        val params = ArrayBuffer.empty[ParameterTypeDesc]
        n.getParameters.asScala.foreach { v =>
          val pName = v.getName
          val pType = v.getType
          val notNull = pType.getAnnotations.asScala
            .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
          val typeDesc = parseType(arg, pType)
          params.append(ParameterTypeDesc(pName.asString(), typeDesc, notNull))
        }
        constructorDesc = Some(ConstructorDesc(params.result.toVector))
      }
      super.visit(n, arg)
    }

    override def visit(n: JavadocComment, arg: RESULT): Unit = {
      classJavadocText = Some(n.parse().getDescription.toText)
      super.visit(n, arg)
    }

    override def visit(n: MethodDeclaration, arg: RESULT): Unit = {
      arg.logger.debug(s"counter = ${counter}, ${n}")
      counter += 1

      val currentTypeName = n.getParentNode.get.accept(new ClassVisitor(), arg)
      // arg.logger.debug(s"currentTypeName = ${currentTypeName}, $typeName, counter = $counter")

      if (currentTypeName == typeName && !n.getModifiers.contains(Modifier.privateModifier()) && !n.getModifiers
            .contains(
              Modifier.protectedModifier()
            )) {
        val obj        = n.asMethodDeclaration()
        val isStatic   = obj.isStatic
        val isThrows   = obj.getThrownExceptions.isNonEmpty
        val methodName = obj.getName.asString()
        val params     = ArrayBuffer.empty[ParameterTypeDesc]
        obj.getParameters.asScala.foreach { v =>
          val pName = v.getName
          val pType = v.getType
          val notNull = pType.getAnnotations.asScala
            .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
          val typeDesc = parseType(arg, pType)
          params.append(ParameterTypeDesc(pName.asString(), typeDesc, notNull))
        }
        val returnTypeName = parseType(arg, obj.getType)
        val notNull =
          obj.getAnnotations.asScala
            .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
        methodDescs.append(
          MethodDesc(methodName, params.result.toVector, returnTypeName, notNull, isThrows, isStatic)
        )
      }
      super.visit(n, arg)
    }

  }

}
