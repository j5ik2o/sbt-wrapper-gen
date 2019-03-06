package com.github.j5ik2o.sbt.wrapper.gen

import java.io.FileWriter

import com.github.j5ik2o.sbt.wrapper.gen.model.TypeDesc
import com.github.j5ik2o.sbt.wrapper.gen.util.Loan._
import com.github.javaparser.{JavaParser, ParserConfiguration}
import sbt.complete.Parser
import sbt.{File, _}

import scala.util.{Success, Try}

trait WrapperGen {



  import complete.DefaultParsers._

  private val oneStringParser: Parser[String] = token(Space ~> StringBasic, "table name")

  private val manyStringParser: Parser[Seq[String]] = token(Space ~> StringBasic, "table name") +

  case class GeneratorContext(logger: Logger,
                              classNameFilter: String => Boolean,
                              typeNameMapper: String => Seq[String],
                              templateDirectory: File,
                              templateNameMapper: String => String,
                              outputDirectoryMapper: String => File,
                              parserConfigurationOpt: Option[ParserConfiguration]) {
    val javaParser = parserConfigurationOpt.map(pc => new JavaParser(pc)).getOrElse(new JavaParser())
  }

  /**
    * テーブル名を指定してファイルを生成する。
    *
    * @param tableName テーブル名
    * @param ctx       [[GeneratorContext]]
    * @return 生成されたSeq[File]
    */
  private[gen] def generateOne(className: String)(implicit ctx: GeneratorContext): Try[Seq[File]] = {
    implicit val logger = ctx.logger
    logger.debug(s"generateOne: start")
    val result = for {
      cfg        <- createTemplateConfiguration(ctx.templateDirectory)
      classDescs <- getClassDescs(ctx.javaParser)
      files <- classDescs
        .filter { classDesc =>
          ctx.classNameFilter(classDesc.name)
        }
        .find(_.name == className)
        .map { tableDesc =>
          generateFiles(cfg, tableDesc)
        }
        .get
    } yield files
    logger.debug(s"generateOne: finished = $result")
    result
  }

  private[gen] def getClassDescs(javaParser: JavaParser)(implicit logger: Logger): Try[Seq[TypeDesc]] = {
    logger.debug(s"getTableDescs(): start")

    logger.debug(s"getTableDescs: finished = ")
    ???
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

  private[generator] def generateFiles(cfg: freemarker.template.Configuration,
                                       typeDesc: TypeDesc)(implicit ctx: GeneratorContext): Try[Seq[File]] = {
    implicit val logger = ctx.logger
    logger.debug(s"generateFiles($cfg, $typeDesc): start")
    val result = ctx
      .typeNameMapper(typeDesc.name)
      .foldLeft(Try(Seq.empty[File])) { (result, className) =>
        val outputTargetDirectory = ctx.outputDirectoryMapper(className)
        for {
          r <- result
          file <- generateFile(cfg, typeDesc, className, outputTargetDirectory)
        } yield {
          r :+ file
        }
      }
    logger.debug(s"generateFiles: finished = $result")
    result
  }

  private[gen] def generateFile(
      cfg: freemarker.template.Configuration,
      typeDesc: TypeDesc, className: String,
      outputDirectory: File
  )(implicit ctx: GeneratorContext): Try[File] = {
    implicit val logger = ctx.logger
    logger.debug(s"generateFile($cfg, $typeDesc, $className, $outputDirectory): start")
    val templateName = ctx.templateNameMapper(className)
    val template     = cfg.getTemplate(templateName)
    val file         = createFile(outputDirectory, className)
    ctx.logger.info(
      s"typeDesc = $typeDesc, className = $className, templateName = $templateName, generate file = $file"
    )

    if (!outputDirectory.exists())
      IO.createDirectory(outputDirectory)

    val result = using(new FileWriter(file)) { writer =>
      //val context = createContext(primaryKeys, columns, tableDesc.tableName, className)
      template.process(null, writer)
      writer.flush()
      Success(file)
    }
    logger.debug(s"generateFile: finished = $result")
    result
  }

}
