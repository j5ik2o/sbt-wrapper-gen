package com.github.j5ik2o.sbt.wrapper.gen

import com.github.j5ik2o.sbt.wrapper.gen.model._
import org.scalatest.{ FreeSpec, Matchers }
import sbt.internal.util.ConsoleLogger

import scala.util.Success

class WrapperGenSpec extends FreeSpec with Matchers with WrapperGen {
  implicit val consoleLogger = ConsoleLogger()
  "WrapperGen" - {
    "getClassDescs" in {
      val _classFilter = { _: TypeDesc =>
        true
      }
      val context = GeneratorContext(
        logger = consoleLogger,
        classFilter = _classFilter,
        typeDescMapper = WrapperGen.defaultTypeDescMapper,
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
              Vector(ParameterTypeDesc("firstName", StringTypeDesc(), notNull = false),
                     ParameterTypeDesc("lastName", StringTypeDesc(), notNull = false))
            ),
            Vector(MethodDesc("getFirstName", Vector(), StringTypeDesc(), notNull = true),
                   MethodDesc("getLastName", Vector(), StringTypeDesc(), notNull = true)),
            Some("example")
          )
        )
      )

    }
  }

}
