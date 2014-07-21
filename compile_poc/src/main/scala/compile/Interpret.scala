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
  settings.Yreplsync.value = true; // make error logs disappear. Dirty hack!

  val loop = new ILoop with RecursiveFunctionsInterpreter
  loop.silent = true;

  def main(args: Array[String]) {
    if (args.length != 0) {
      //      println("Setting from command line");
      loop.process(args) // setting REPL from command line

    } else {
      //      println("Setting default");
      loop.process(settings)
    }
    loop.loop
  }

}