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
import org.seasar.util.lang.{ ClassLoaderUtil, ClassUtil }
import sbt.Keys._
import sbt.complete.Parser
import sbt.{ Def, File, _ }

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.compat.java8.OptionConverters
import scala.compat.java8.StreamConverters._
import scala.util.{ Success, Try }

object WrapperGen {
  type ClassFilter           = TypeDesc => Boolean
  type TypeDescMapper        = (String, Seq[TypeDesc]) => TypeDesc
  type TemplateNameMapper    = TypeDesc => String
  type OutputDirectoryMapper = TypeDesc => File

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

trait WrapperGen {

  import SbtWrapperGenKeys._
  import WrapperGen._

  type RESULT = GeneratorContext

  import complete.DefaultParsers._

  private val oneStringParser: Parser[String] = token(Space ~> StringBasic, "java class name")

  private val manyStringParser: Parser[Seq[String]] = token(Space ~> StringBasic, "java class name") +

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

  def parseType(t: Type, typeMapper: TypeDescMapper): TypeDesc = {
    t match {
      case t if t.isArrayType =>
        val at        = t.asArrayType()
        val paramType = parseType(at.getElementType, typeMapper)
        typeMapper("Array", Seq(paramType))
      case t if t.isClassOrInterfaceType =>
        val cit      = t.asClassOrInterfaceType()
        val typeName = cit.getName.getIdentifier
        val ta = OptionConverters
          .toScala(cit.getTypeArguments).map { typeArguments =>
            typeArguments.asScala.map { t =>
              parseType(t, typeMapper)
            }
          }.getOrElse(Seq.empty)
        typeMapper(typeName, ta)
      case t if t.isIntersectionType => null
      case t if t.isPrimitiveType =>
        val pt       = t.asPrimitiveType()
        val typeName = if (pt.asString() == "Integer") "Int" else pt.asString
        typeMapper(typeName, Seq.empty)
      case t if t.isReferenceType => null
      case t if t.isTypeParameter => null
      case t if t.isUnionType     => null
      case t if t.isUnknownType   => null
      case t if t.isVarType       => null
      case t if t.isVoidType      => VoidTypeDesc()
      case t if t.isWildcardType  => null
    }
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
        val typeDesc = parseType(pType, arg.typeDescMapper)
        params.append(ParameterTypeDesc(pName.asString(), typeDesc, notNull))
      }
      constructorDesc = ConstructorDesc(params.result.toVector)
      super.visit(n, arg)
    }

    override def visit(n: MethodDeclaration, arg: RESULT): Unit = {
      if (n.getModifiers.contains(Modifier.publicModifier())) {
        val obj        = n.asMethodDeclaration()
        val methodName = obj.getName.asString()
        val params     = ArrayBuffer.empty[ParameterTypeDesc]
        obj.getParameters.asScala.foreach { v =>
          val pName = v.getName
          val pType = v.getType
          val notNull = pType.getAnnotations.asScala
            .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
          val typeDesc = parseType(pType, arg.typeDescMapper)
          params.append(ParameterTypeDesc(pName.asString(), typeDesc, notNull))
        }
        val returnTypeName = parseType(obj.getType, arg.typeDescMapper)
        val notNull =
          obj.getAnnotations.asScala
            .map(_.getName.asString().toLowerCase()).exists(v => v == "nonnull" || v == "notnull")
        methodDescs.append(
          MethodDesc(methodName, params.result.toVector, returnTypeName, notNull)
        )
      }
      super.visit(n, arg)
    }
  }

  def generateOneTask: Def.Initialize[InputTask[Seq[File]]] = Def.inputTask {
    val className       = oneStringParser.parsed
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateOne task")

    val classLoader =
      if ((enableManagedClassPath in scalaWrapperGen).value)
        ClasspathUtilities.toLoader(
          (managedClasspath in Compile).value.map(_.data),
          ClasspathUtilities.xsbtiLoader
        )
      else
        ClasspathUtilities.xsbtiLoader

    val classFilterValue           = (typeDescFilter in scalaWrapperGen).value
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

  def generateManyTask: Def.Initialize[InputTask[Seq[File]]] = Def.inputTask {
    val classNames      = manyStringParser.parsed
    implicit val logger = streams.value.log
    logger.info("sbt-wrapper-gen: generateOne task")

    val classLoader =
      if ((enableManagedClassPath in scalaWrapperGen).value)
        ClasspathUtilities.toLoader(
          (managedClasspath in Compile).value.map(_.data),
          ClasspathUtilities.xsbtiLoader
        )
      else
        ClasspathUtilities.xsbtiLoader

    val classFilterValue           = (typeDescFilter in scalaWrapperGen).value
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

  def generateAllTask: Def.Initialize[Task[Seq[File]]] = Def.taskDyn {
    val managedClasspathValue = (managedClasspath in Compile).value
    Def.task {
      implicit val logger = streams.value.log
      logger.info("sbt-wrapper-gen: generateAll task")

      val classLoader =
        if ((enableManagedClassPath in scalaWrapperGen).value)
          ClasspathUtilities.toLoader(
            managedClasspathValue.map(_.data),
            ClasspathUtilities.xsbtiLoader
          )
        else
          ClasspathUtilities.xsbtiLoader

      val classFilterValue           = (typeDescFilter in scalaWrapperGen).value
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
      _generateAll(context).get
    }
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
        .find(_.simpleTypeName == className)
        .map { classDesc =>
          generateFiles(context, cfg, classDesc)
        }
        .get
    } yield files
    logger.debug(s"generateOne: finished = $result")
    result
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
          classNames.contains(classDesc.simpleTypeName)
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

  private[gen] def getClassDescs(context: GeneratorContext): Try[Seq[TypeDesc]] = Try {
    context.logger.debug(s"getClassDescs($context): start")
    def loop(path: Path): Seq[TypeDesc] = {
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

  private[gen] def createFile(outputDirectory: File, className: String)(
      implicit logger: Logger
  ): File = {
    logger.debug(s"createFile($outputDirectory, $className): start")
    val file = outputDirectory / (className + ".scala")
    logger.debug(s"createFile: finished = $file")
    file
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
                                 typeDesc: TypeDesc): Try[Seq[File]] = {
    implicit val logger = context.logger
    logger.debug(s"generateFiles($cfg, $typeDesc): start")
    val outputTargetDirectory = context.outputDirectoryMapper(typeDesc)
    val result                = generateFile(context, cfg, typeDesc, typeDesc.simpleTypeName, outputTargetDirectory).map(Seq(_))
    logger.debug(s"generateFiles: finished = $result")
    result
  }

  private[gen] def generateFile(context: GeneratorContext,
                                cfg: freemarker.template.Configuration,
                                typeDesc: TypeDesc,
                                className: String,
                                outputDirectory: File): Try[File] = {
    implicit val logger = context.logger
    logger.debug(s"generateFile($cfg, $typeDesc, $className, $outputDirectory): start")
    val templateName = context.templateNameMapper(typeDesc)
    val template     = cfg.getTemplate(templateName)
    val file         = createFile(outputDirectory, className)
    context.logger.info(
      s"typeDesc = $typeDesc, className = $className, templateName = $templateName, generate file = $file"
    )

    if (!outputDirectory.exists())
      IO.createDirectory(outputDirectory)

    val result = using(new FileWriter(file)) { writer =>
      //val context = createContext(primaryKeys, columns, tableDesc.tableName, className)
      template.process(typeDesc.asMap, writer)
      writer.flush()
      Success(file)
    }
    logger.debug(s"generateFile: finished = $result")
    result
  }

}
