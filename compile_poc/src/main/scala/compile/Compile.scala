package compile

import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Settings
import scala.tools.nsc.Global
import scala.reflect.io.File
import ch.epfl.lamp.compiler.msil.Type
import scala.reflect.internal.Types
import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.Trees
import scala.tools.nsc.typechecker.Analyzer
import scala.util.Try

object Compile {
  val settings = new Settings
  settings.bootclasspath.value = libPath.mkString(File.pathSeparator)
  settings.classpath.value = libPath.mkString(File.pathSeparator)
  println(settings.classpath);
  println(settings.bootclasspath);
  val reporter = new ConsoleReporter(settings)
  val global = new Global(settings, reporter) with RecursiveFunctions

  def main(args: Array[String]) {
    import scala.reflect.runtime.universe._

    val file = scala.reflect.io.AbstractFile.getFile("../../factorial.scalatest")
    val comp = new global.Run

    //    comp.compileFiles(List(file))  //runs compilation immediately
    comp.compileLate(file)

    //execute phases one by one
    var phase = comp.parserPhase
    phase.run
    comp.advancePhase
    do {
      phase = phase.next;
      println("-------------------------------------")
      println(phase);
      println("-------------------------------------")
      phase.run
      comp.advancePhase
    } while (phase.hasNext)

  }

  private lazy val libPath = try {
    classPathOfClass("scala.ScalaObject")
  } catch {
    case e: Throwable =>
      throw new RuntimeException("Unable to load scala base object from classpath (scala-library jar is missing?)", e)
  }

  private def classPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    if (path.indexOf("file:") >= 0) {
      val indexOfFile = path.indexOf("file:") + 5
      val indexOfSeparator = path.lastIndexOf('!')
      List(path.substring(indexOfFile, indexOfSeparator))
    } else {
      require(path.endsWith(resource))
      List(path.substring(0, path.length - resource.length + 1))
    }
  }
}