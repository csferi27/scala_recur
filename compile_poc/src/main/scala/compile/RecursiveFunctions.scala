package compile

import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.interpreter.ReplGlobal
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.ReplReporter

trait RecursiveFunctions extends Global {
  override lazy val analyzer = new {
    override val global: this.type = RecursiveFunctions.this
  } with MyAnalyzer
}

trait RecursiveFunctionsInterpreter extends ILoop {
  trait ReporterAdvice extends Reporter {
    import scala.reflect.internal.util._
    abstract override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
      println("Info called!");
      println("pos:" + pos);
      println("msg:" + msg);
      super.info0(pos, msg, severity, force);
    }
  }

  override def createInterpreter() {
    if (addedClasspath != "")
      settings.classpath append addedClasspath

    intp = new ILoopInterpreter {
      override lazy val reporter = new ReplReporter(this) with ReporterAdvice
      override protected def newCompiler(settings: Settings, reporter: Reporter): ReplGlobal = {
        settings.outputDirs setSingleOutput virtualDirectory
        settings.exposeEmptyPackage.value = true
        new Global(settings, reporter) with ReplGlobal with RecursiveFunctions {
          override def toString: String = "<global>"
        }
      }
    }
  }
}