package com.github.j5ik2o.sbt.wrapper.gen

import com.github.j5ik2o.sbt.wrapper.gen.model._
import org.scalatest.{ FreeSpec, Matchers }
import sbt.internal.util.ConsoleLogger

class WrapperGeneratorSpec extends FreeSpec with Matchers with WrapperGenerator {
  implicit val consoleLogger = ConsoleLogger()
  "WrapperGen" - {
    "getTypeDescs" in {
      val _classFilter = { _: TypeDesc =>
        true
      }
      val context = GeneratorContext(
        logger = consoleLogger,
        typeFilter = _classFilter,
        typeDescMapper = WrapperGenerator.defaultTypeDescMapper,
        templateDirectories = null,
        templateNameMapper = null,
        inputDirectory = sbt.file("src/test/java/example"),
        outputDirectoryMapper = null,
        typeNameMapper = { v =>
          Seq(v.simpleTypeName)
        },
        packageNameMapper = {
          case (v, _, _) =>
            v
        },
        parserConfigurationOpt = None
      )
      val r = getTypeDescs(context)()
      println(r.get.map(_.asJavaMap))
      println(r.get.map(_.asScalaDesc.asJavaMap))
      //      getTypeDescs(context)() shouldBe Success(
//        Vector(
//          ClassDesc(
//            "Customer",
//            Some(
//              ConstructorDesc(
//                Vector(ParameterTypeDesc("firstName", StringTypeDesc(), false),
//                       ParameterTypeDesc("lastName", StringTypeDesc(), false))
//              )
//            ),
//            Vector(
//              MethodDesc("setLastName",
//                         Vector(ParameterTypeDesc("lastName", StringTypeDesc(), false)),
//                         VoidTypeDesc(),
//                         false,
//                         true,
//                         false),
//              MethodDesc("getFirstName", Vector(), StringTypeDesc(), true, false, false),
//              MethodDesc("getLastName", Vector(), StringTypeDesc(), false, false, false)
//            ),
//            Vector(FieldDesc("firstName", StringTypeDesc(), false, false),
//                   FieldDesc("lastName", StringTypeDesc(), false, false)),
//            Paths.get("src/test/java/example/Customer.java"),
//            false,
//            false,
//            Some("example")
//          )
//        )
//      )

    }
  }

}
