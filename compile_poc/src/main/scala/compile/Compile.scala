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
  settings.bootclasspath.value = Utils.libPath.mkString(File.pathSeparator)
  settings.classpath.value = Utils.libPath.mkString(File.pathSeparator)

  val reporter = new ConsoleReporter(settings) with ReporterSilentAdvice
  reporter.silent = true; // make error logs disappear. Dirty hack!

  val global = new Global(settings, reporter) with RecursiveFunctionsGlobal

  def main(args: Array[String]) {
    val file = scala.reflect.io.AbstractFile.getFile(new java.io.File(".").getCanonicalPath + "/testcases.txt")
    import scala.reflect.runtime.universe._

    //    compileAllPhasesAtOnce(file)
    compilePhaseByPhase(file)
  }

  private def compileAllPhasesAtOnce(file: scala.reflect.io.AbstractFile): Unit = {
    val compileRun = new global.Run
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

}
