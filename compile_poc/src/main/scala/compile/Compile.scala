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
  val reporter = new ConsoleReporter(settings)
  val global = new Global(settings, reporter) with RecursiveFunctions

  def main(args: Array[String]) {
    import scala.reflect.runtime.universe._

    val file = scala.reflect.io.AbstractFile.getFile("/home/csajka/svn_repos/scala_recur/scala_recur/compile_poc/factorial.scalatest")
    //    compileAllPhasesAtOnce(file)
    compilePhaseByPhase(file)
  }

  private def compileAllPhasesAtOnce(file: scala.reflect.io.AbstractFile): Unit = {
    val compileRun = new global.Run
    //    global.afterEachPhase(global.)
    compileRun.compileFiles(List(file)) //runs compilation immediately
  }

  private def compilePhaseByPhase(file: scala.reflect.io.AbstractFile): Unit = {
    val compileRun = new global.Run
    compileRun.compileLate(file)

    global.phase = compileRun.parserPhase
    global.phase.run
    compileRun.advancePhase
    do {
      global.phase = global.phase.next;
      println("-------------------------------------")
      println(global.phase);
      println("-------------------------------------")
      global.phase.run
      compileRun.advancePhase
    } while (global.phase.hasNext)
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