package com.github.j5ik2o.sbt.wrapper.gen

import java.io.FileWriter
import java.nio.file.{ Files, Path }

import com.github.j5ik2o.sbt.wrapper.gen.model._
import com.github.j5ik2o.sbt.wrapper.gen.util.Loan._
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.visitor.VoidVisitorAdapter
import com.github.javaparser.ast.{ Modifier, PackageDeclaration }
import com.github.javaparser.{ JavaParser, ParserConfiguration }
import org.seasar.util.io.FileUtil
import sbt.Keys._
import sbt.complete.Parser
import sbt.{ Def, File, _ }

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.compat.java8.OptionConverters
import scala.compat.java8.StreamConverters._
import scala.util.{ Success, Try }

object WrapperGenerator {
  type ClassFilter           = ClassDesc => Boolean
  type TypeDescMapper        = (String, Seq[TypeDesc]) => TypeDesc
  type TemplateNameMapper    = ClassDesc => String
  type OutputDirectoryMapper = ClassDesc => File

  val defaultTypeDescMapper: TypeDescMapper = {
    case ("String", _) =>
      StringTypeDesc()
    case ("Array", Seq(valueType)) =>
      ArrayTypeDesc(valueType)
    case ("List", Seq(valueType)) =>
      JavaListTypeDesc(valueType)
    case ("Map", Seq(keyType, valueType)) =>
      JavaMapTypeDesc(keyType, valueType)
    case ("CompletableFuture", Seq(paramType)) =>
      CompletableFutureDesc(paramType)
    case ("Integer", _) =>
      PrimitiveTypeDesc(PrimitiveType.INT)
    case ("Log", _) =>
      PrimitiveTypeDesc(PrimitiveType.LONG)
    case ("Short", _) =>
      PrimitiveTypeDesc(PrimitiveType.SHORT)
    case ("Boolean", _) =>
      PrimitiveTypeDesc(PrimitiveType.BOOLEAN)
    case ("Byte", _) =>
      PrimitiveTypeDesc(PrimitiveType.BYTE)
    case ("Char", _) =>
      PrimitiveTypeDesc(PrimitiveType.CHAR)
    case ("Double", _) =>
      PrimitiveTypeDesc(PrimitiveType.DOUBLE)
    case ("Float", _) =>
      PrimitiveTypeDesc(PrimitiveType.FLOAT)
    case (other, paramTypes) =>
      OtherTypeDesc(other, paramTypes)
  }
}

trait WrapperGenerator {

  import SbtWrapperGenKeys._
  import WrapperGenerator._

  type RESULT = GeneratorContext

  import complete.DefaultParsers._

  private val oneStringParser: Parser[String] = token(Space ~> StringBasic, "java class name")

  private val manyStringParser: Parser[Seq[String]] = token(Space ~> StringBasic, "java class name") +

  def parseType(context: GeneratorContext, t: Type, typeMapper: TypeDescMapper): TypeDesc = {
    t match {
      case t if t.isArrayType =>
        val at        = t.asArrayType()
        val paramType = parseType(context, at.getElementType, typeMapper)
        typeMapper("Array", Seq(paramType))
      case t if t.isClassOrInterfaceType =>
        val cit      = t.asClassOrInterfaceType()
        val typeName = cit.getName.getIdentifier
        val ta = OptionConverters
          .toScala(cit.getTypeArguments).map { typeArguments =>
            typeArguments.asScala.flatMap { t =>
              val result = parseType(context, t, typeMapper)
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
        null
    }
  }

  def generateOneTask: Def.Initialize[InputTask[Seq[File]]] = Def.inputTask {
    val className       = oneStringParser.parsed
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateOne task")

    val classFilterValue           = (classDescFilter in scalaWrapperGen).value
    val typeDescMapperValue        = (typeDescMapper in scalaWrapperGen).value
    val templateDirectoryValue     = (templateDirectory in scalaWrapperGen).value
    val templateNameMapperValue    = (templateNameMapper in scalaWrapperGen).value
    val inputDirectoryValue        = (inputSourceDirectory in scalaWrapperGen).value
    val outputDirectoryMapperValue = (outputSourceDirectoryMapper in scalaWrapperGen).value
    val parserConfigurationValue   = (javaParserConfiguration in scalaWrapperGen).value

    val ctx: GeneratorContext = GeneratorContext(
      logger = streams.value.log,
      classFilter = classFilterValue,
      typeDescMapper = typeDescMapperValue,
      templateDirectory = templateDirectoryValue,
      templateNameMapper = templateNameMapperValue,
      inputDirectory = inputDirectoryValue,
      outputDirectoryMapper = outputDirectoryMapperValue,
      parserConfigurationOpt = parserConfigurationValue
    )
    _generateOne(ctx, className).get
  }

  private[gen] def _generateOne(context: GeneratorContext, className: String): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateOne: start")
    val result: Try[Seq[File]] = for {
      cfg        <- createTemplateConfiguration(context.templateDirectory)
      classDescs <- getClassDescs(context)
      files <- classDescs
        .filter { classDesc =>
          context.classFilter(classDesc)
        }
        .find { v =>
          containClassName(className, v)
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
    val classNames      = manyStringParser.parsed
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateOne task")

    val classFilterValue           = (classDescFilter in scalaWrapperGen).value
    val typeDescMapperValue        = (typeDescMapper in scalaWrapperGen).value
    val templateDirectoryValue     = (templateDirectory in scalaWrapperGen).value
    val templateNameMapperValue    = (templateNameMapper in scalaWrapperGen).value
    val inputDirectoryValue        = (inputSourceDirectory in scalaWrapperGen).value
    val outputDirectoryMapperValue = (outputSourceDirectoryMapper in scalaWrapperGen).value
    val parserConfigurationValue   = (javaParserConfiguration in scalaWrapperGen).value

    val context: GeneratorContext = GeneratorContext(
      logger = streams.value.log,
      classFilter = classFilterValue,
      typeDescMapper = typeDescMapperValue,
      templateDirectory = templateDirectoryValue,
      templateNameMapper = templateNameMapperValue,
      inputDirectory = inputDirectoryValue,
      outputDirectoryMapper = outputDirectoryMapperValue,
      parserConfigurationOpt = parserConfigurationValue
    )
    _generateMany(context, classNames).get
  }

  private[gen] def _generateMany(context: GeneratorContext, classNames: Seq[String]): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateMany($classNames): start")
    val result = for {
      cfg        <- createTemplateConfiguration(context.templateDirectory)
      classDescs <- getClassDescs(context)
      files <- classDescs
        .filter { classDesc =>
          context.classFilter(classDesc)
        }
        .filter { classDesc =>
          classNames.exists { cn =>
            containClassName(cn, classDesc)
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

  private def containClassName(className: String, classDescv: ClassDesc): Boolean = {
    classDescv.simpleTypeName == className || classDescv.packageName.fold[String](classDescv.simpleTypeName) { p =>
      s"$p.${classDescv.simpleTypeName}"
    } == className
  }

  private[gen] def getClassDescs(context: GeneratorContext): Try[Seq[ClassDesc]] = Try {
    context.logger.debug(s"getClassDescs($context): start")
    def loop(path: Path): Seq[ClassDesc] = {
      val files = Files.list(path).toScala
      context.logger.debug(s"files = $files")
      val r = files.filterNot(v => v.endsWith("package-info.java")).flatMap { path =>
        if (path.toFile.isDirectory) { loop(path) } else {
          context.logger.debug(s"path = $path")
          val compileUnit = context.javaParser.parse(path)
          val cpResult    = compileUnit.getResult
          context.logger.debug(s"cpResult = $cpResult")
          val resultOpt = OptionConverters.toScala(cpResult)
          resultOpt
            .map { result =>
              val visitor = new Visitor()
              result.accept(visitor, context)
              Seq(visitor.result)
            }.getOrElse(Seq.empty)
        }
      }
      r
    }
    val r = loop(context.inputDirectory.toPath)
    context.logger.debug(s"getClassDescs: finished = $r")
    r
  }

  private[gen] def createTemplateConfiguration(
      templateDirectory: File
  )(implicit logger: Logger): Try[freemarker.template.Configuration] = Try {
    logger.debug(s"createTemplateConfiguration($templateDirectory): start")
    var cfg: freemarker.template.Configuration = null
    try {
      cfg = new freemarker.template.Configuration(
        freemarker.template.Configuration.DEFAULT_INCOMPATIBLE_IMPROVEMENTS
      )
      cfg.setDirectoryForTemplateLoading(templateDirectory)
    } finally {
      logger.debug(s"createTemplateConfiguration: finished = $cfg")
    }
    cfg
  }

  private[gen] def generateFiles(context: GeneratorContext,
                                 cfg: freemarker.template.Configuration,
                                 classDesc: ClassDesc): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateFiles($cfg, $classDesc): start")
    val outputTargetDirectory = context.outputDirectoryMapper(classDesc)
    val result                = generateFile(context, cfg, classDesc, classDesc.simpleTypeName, outputTargetDirectory).map(Seq(_))
    logger.debug(s"generateFiles: finished = $result")
    result
  }

  private[gen] def generateFile(context: GeneratorContext,
                                cfg: freemarker.template.Configuration,
                                classDesc: ClassDesc,
                                className: String,
                                outputDirectory: File): Try[File] = {
    implicit val logger = context.logger
    logger.debug(s"generateFile($cfg, $classDesc, $className, $outputDirectory): start")
    val templateName = context.templateNameMapper(classDesc)
    val template     = cfg.getTemplate(templateName)
    val file         = createFile(outputDirectory, className)
    context.logger.debug(
      s"classDesc = $classDesc, className = $className, templateName = $templateName, generate file = $file"
    )

    if (!outputDirectory.exists())
      IO.createDirectory(outputDirectory)

    val result = using(new FileWriter(file)) { writer =>
      template.process(classDesc.asMap, writer)
      writer.flush()
      Success(file)
    }
    logger.debug(FileUtil.readText(file))
    logger.debug(s"generateFile: finished = $result")
    result
  }

  private[gen] def createFile(outputDirectory: File, className: String)(
      implicit logger: Logger
  ): File = {
    logger.debug(s"createFile($outputDirectory, $className): start")
    val file = outputDirectory / (className + ".scala")
    logger.debug(s"createFile: finished = $file")
    file
  }

  def generateAllTask: Def.Initialize[Task[Seq[File]]] = Def.task {
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateAll task")

    val classDescToBoolean         = (classDescFilter in scalaWrapperGen).value
    val typeDescMapperValue        = (typeDescMapper in scalaWrapperGen).value
    val templateDirectoryValue     = (templateDirectory in scalaWrapperGen).value
    val templateNameMapperValue    = (templateNameMapper in scalaWrapperGen).value
    val inputDirectoryValue        = (inputSourceDirectory in scalaWrapperGen).value
    val outputDirectoryMapperValue = (outputSourceDirectoryMapper in scalaWrapperGen).value
    val parserConfigurationValue   = (javaParserConfiguration in scalaWrapperGen).value

    val context: GeneratorContext = GeneratorContext(
      logger = streams.value.log,
      classFilter = classDescToBoolean,
      typeDescMapper = typeDescMapperValue,
      templateDirectory = templateDirectoryValue,
      templateNameMapper = templateNameMapperValue,
      inputDirectory = inputDirectoryValue,
      outputDirectoryMapper = outputDirectoryMapperValue,
      parserConfigurationOpt = parserConfigurationValue
    )
    _generateAll(context).get
  }

  private[gen] def _generateAll(context: GeneratorContext): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateAll: start")
    val result = for {
      cfg        <- createTemplateConfiguration(context.templateDirectory)
      classDescs <- getClassDescs(context)
      files <- classDescs
        .filter { classDesc =>
          context.classFilter(classDesc)
        }
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
                              classFilter: ClassFilter,
                              typeDescMapper: TypeDescMapper,
                              templateDirectory: File,
                              templateNameMapper: TemplateNameMapper,
                              inputDirectory: File,
                              outputDirectoryMapper: OutputDirectoryMapper,
                              parserConfigurationOpt: Option[ParserConfiguration]) {
    val javaParser = parserConfigurationOpt.map(pc => new JavaParser(pc)).getOrElse(new JavaParser())
  }

  class Visitor extends VoidVisitorAdapter[RESULT] {

    private val methodDescs                      = ArrayBuffer.empty[MethodDesc]
    private var className: String                = _
    private var packageName: Option[String]      = None
    private var constructorDesc: ConstructorDesc = _

    def result: ClassDesc = {
      val result = ClassDesc(className, constructorDesc, methodDescs.result.toVector, packageName)
      result
    }

    override def visit(n: PackageDeclaration, arg: RESULT): Unit = {
      packageName = Some(n.getName.getIdentifier)
      super.visit(n, arg)
    }

    override def visit(n: ClassOrInterfaceDeclaration, arg: RESULT): Unit = {
      val name = n.asClassOrInterfaceDeclaration().getName
      className = name.asString()
      super.visit(n, arg)
    }

    override def visit(n: ConstructorDeclaration, arg: RESULT): Unit = {
      val params = ArrayBuffer.empty[ParameterTypeDesc]
      n.getParameters.asScala.foreach { v =>
        val pName = v.getName
        val pType = v.getType
        val notNull = pType.getAnnotations.asScala
          .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
        val typeDesc = parseType(arg, pType, arg.typeDescMapper)
        params.append(ParameterTypeDesc(pName.asString(), typeDesc, notNull))
      }
      constructorDesc = ConstructorDesc(params.result.toVector)
      super.visit(n, arg)
    }

    override def visit(n: MethodDeclaration, arg: RESULT): Unit = {
      if (n.getModifiers.contains(Modifier.publicModifier())) {
        val obj        = n.asMethodDeclaration()
        val isThrows   = obj.getThrownExceptions.isNonEmpty
        val methodName = obj.getName.asString()
        val params     = ArrayBuffer.empty[ParameterTypeDesc]
        obj.getParameters.asScala.foreach { v =>
          val pName = v.getName
          val pType = v.getType
          val notNull = pType.getAnnotations.asScala
            .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
          val typeDesc = parseType(arg, pType, arg.typeDescMapper)
          params.append(ParameterTypeDesc(pName.asString(), typeDesc, notNull))
        }
        val returnTypeName = parseType(arg, obj.getType, arg.typeDescMapper)
        val notNull =
          obj.getAnnotations.asScala
            .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
        methodDescs.append(
          MethodDesc(methodName, params.result.toVector, returnTypeName, notNull, isThrows)
        )
      }
      super.visit(n, arg)
    }
  }

}
