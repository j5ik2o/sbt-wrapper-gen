package com.github.j5ik2o.sbt.wrapper.gen

import java.io.File

import sbt.internal.inc.classpath.{ ClasspathUtilities => CU }

object ClasspathUtilities {

  def toLoader(paths: Seq[File], parent: ClassLoader): ClassLoader =
    CU.toLoader(paths, parent)

  lazy val xsbtiLoader: ClassLoader = CU.xsbtiLoader

}
