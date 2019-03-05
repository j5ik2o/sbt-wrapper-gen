package com.github.j5ik2o.sbt.wrapper.gen

import com.github.j5ik2o.sbt.wrapper.gen.model._
import org.scalatest.{ FreeSpec, Matchers }
import sbt.internal.util.ConsoleLogger

import scala.util.Success

class WrapperGeneratorSpec extends FreeSpec with Matchers with WrapperGenerator {
  implicit val consoleLogger = ConsoleLogger()
  "WrapperGen" - {
    "getClassDescs" in {
      val _classFilter = { _: TypeDesc =>
        true
      }
      val context = GeneratorContext(
        logger = consoleLogger,
        classFilter = _classFilter,
        typeDescMapper = WrapperGenerator.defaultTypeDescMapper,
        templateDirectory = null,
        templateNameMapper = null,
        inputDirectory = sbt.file("src/test/java/example"),
        outputDirectoryMapper = null,
        parserConfigurationOpt = None
      )
      getClassDescs(context) shouldBe Success(
        Vector(
          ClassDesc(
            "Customer",
            ConstructorDesc(
              Vector(ParameterTypeDesc("firstName", StringTypeDesc(), false),
                     ParameterTypeDesc("lastName", StringTypeDesc(), false))
            ),
            Vector(
              MethodDesc("setLastName",
                         Vector(ParameterTypeDesc("lastName", StringTypeDesc(), false)),
                         VoidTypeDesc(),
                         false,
                         true),
              MethodDesc("getFirstName", Vector(), StringTypeDesc(), true, false),
              MethodDesc("getLastName", Vector(), StringTypeDesc(), false, false)
            ),
            Some("example")
          )
        )
      )

    }
  }

}