package compile

import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.interpreter.ReplGlobal
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.ReplReporter

trait RecursiveFunctionsGlobal extends Global {
  override lazy val analyzer = new {
    override val global: this.type = RecursiveFunctionsGlobal.this
  } with CustomAnalyzer
}

trait ReporterSilentAdvice extends Reporter {
  var _silent = false;
  def silent = _silent
  def silent_=(value: Boolean): Unit = { _silent = value }

  import scala.reflect.internal.util._
  abstract override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    if (!silent)
      super.info0(pos, msg, severity, force);
  }
}

trait RecursiveFunctionsInterpreter extends ILoop {
  var _silent = false;
  def silent = _silent
  def silent_=(value: Boolean): Unit = { _silent = value }

  override def createInterpreter() {
    if (addedClasspath != "")
      settings.classpath append addedClasspath

    intp = new ILoopInterpreter {
      override lazy val reporter = {
        val rep = new ReplReporter(this) with ReporterSilentAdvice
        rep.silent = RecursiveFunctionsInterpreter.this.silent
        rep
      }

      override protected def newCompiler(settings: Settings, reporter: Reporter): ReplGlobal = {
        settings.outputDirs setSingleOutput virtualDirectory
        settings.exposeEmptyPackage.value = true
        new Global(settings, reporter) with ReplGlobal with RecursiveFunctionsGlobal {
          override def toString: String = "<global>"
        }
      }
    }
  }
}