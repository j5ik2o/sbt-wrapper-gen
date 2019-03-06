package com.github.j5ik2o.sbt.wrapper.gen

import java.io.FileWriter

import com.github.j5ik2o.sbt.wrapper.gen.model.TypeDesc
import com.github.j5ik2o.sbt.wrapper.gen.util.Loan._
import sbt.complete.Parser
import sbt.{ File, _ }

import scala.util.{ Success, Try }

trait WrapperGen {
  import complete.DefaultParsers._

  private val oneStringParser: Parser[String] = token(Space ~> StringBasic, "table name")

  private val manyStringParser: Parser[Seq[String]] = token(Space ~> StringBasic, "table name") +

  case class GeneratorContext(logger: Logger,
                              classNameFilter: String => Boolean,
                              typeNameMapper: String => String,
                              templateDirectory: File,
                              templateNameMapper: String => String,
                              outputDirectoryMapper: String => File)

  /**
    * テーブル名を指定してファイルを生成する。
    *
    * @param tableName テーブル名
    * @param ctx       [[GeneratorContext]]
    * @return 生成されたSeq[File]
    */
  private[gen] def generateOne(tableName: String)(implicit ctx: GeneratorContext): Try[Seq[File]] = {
    implicit val logger = ctx.logger
    logger.debug(s"generateOne: start")
    val result = for {
      cfg        <- createTemplateConfiguration(ctx.templateDirectory)
      tableDescs <- getTableDescs(ctx.connection, ctx.schemaName)
      files <- tableDescs
        .filter { tableDesc =>
          ctx.tableNameFilter(tableDesc.tableName)
        }
        .find(_.tableName == tableName)
        .map { tableDesc =>
          generateFiles(cfg, tableDesc)
        }
        .get
    } yield files
    logger.debug(s"generateOne: finished = $result")
    result
  }

  private[gen] def getClassDescs()(implicit logger: Logger): Try[Seq[TypeDesc]] = {
    logger.debug(s"getTableDescs($conn, $schemaName): start")
    val result = getTables(conn, schemaName).flatMap { tables =>
      tables.foldLeft(Try(Seq.empty[TableDesc])) { (result, tableName) =>
        for {
          r               <- result
          primaryKeyDescs <- getPrimaryKeyDescs(conn, schemaName, tableName)
          columnDescs     <- getColumnDescs(conn, schemaName, tableName)
        } yield r :+ TableDesc(tableName, primaryKeyDescs, columnDescs)
      }
    }
    logger.debug(s"getTableDescs: finished = $result")
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

  private[gen] def generateFile(
      cfg: freemarker.template.Configuration,
      typeDesc: TypeDesc,
      outputDirectory: File
  )(implicit ctx: GeneratorContext): Try[File] = {
    implicit val logger = ctx.logger
    logger.debug(s"generateFile($cfg, $typeDesc, $outputDirectory): start")
    val templateName = ctx.templateNameMapper(typeDesc.name)
    val template     = cfg.getTemplate(templateName)
    val file         = createFile(outputDirectory, typeDesc.name)
    ctx.logger.info(
      s"typeDesc.name = ${typeDesc.name}, templateName = $templateName, generate file = $file"
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
