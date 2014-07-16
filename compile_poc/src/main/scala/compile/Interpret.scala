package compile

import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.io.File
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.reporters.Reporter

object Interpret {

  val settings = new Settings
  settings.bootclasspath.value = Utils.libPath.mkString(File.pathSeparator)
  settings.classpath.value = Utils.libPath.mkString(File.pathSeparator)
  settings.Yreplsync.value = true;

  val loop = new ILoop with RecursiveFunctionsInterpreter

  def main(args: Array[String]) {
    //    loop.reporter
    loop.process(settings)
    loop.loop
  }

}