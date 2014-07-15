package compile

import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.io.File
import scala.tools.nsc.interpreter.ILoop
object Interpret {

  val settings = new Settings
  settings.bootclasspath.value = libPath.mkString(File.pathSeparator)
  settings.classpath.value = libPath.mkString(File.pathSeparator)
  settings.Yreplsync.value = true;

  val reporter = new ConsoleReporter(settings)

  def main(args: Array[String]) {
    //    val loop = new ILoop
    val loop = new ILoop {
      /** Create a new interpreter. */
      override def createInterpreter() {
        if (addedClasspath != "")
          settings.classpath append addedClasspath

        intp = new ILoopInterpreter with RecursiveFunctionsI
      }
    }
    loop.process(settings)
    //    loop.createInterpreter
    loop.loop
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