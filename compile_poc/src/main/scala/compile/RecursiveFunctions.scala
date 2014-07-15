package compile

import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.interpreter.ReplGlobal
import scala.tools.nsc.interpreter.IMain

trait RecursiveFunctions extends Global {
  override lazy val analyzer = new {
    override val global: this.type = RecursiveFunctions.this
  } with MyAnalyzer
}

trait RecursiveFunctionsI extends IMain {
  override protected def newCompiler(settings: Settings, reporter: Reporter): ReplGlobal = {
    settings.outputDirs setSingleOutput virtualDirectory
    settings.exposeEmptyPackage.value = true
    new Global(settings, reporter) with ReplGlobal with RecursiveFunctions {
      override def toString: String = "<global>"
    }
  }
}