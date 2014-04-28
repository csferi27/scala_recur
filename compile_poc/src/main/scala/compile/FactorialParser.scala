package compile

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

class FactorialParser(val global: Global) extends Plugin {
  import global._

  val name = "factorialparser"
  val description = "sets fact2 type to Any"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with Transform {

    val global: FactorialParser.this.global.type = FactorialParser.this.global
    override val runsAfter = List[String]("namer");
    override val runsBefore = List[String]("typer");
    override val runsRightAfter = Some[String]("parser");
    val phaseName = FactorialParser.this.name

    override def newTransformer(unit: CompilationUnit) = new global.Transformer {

      override def transform(tree: global.Tree) = tree match {
        case defdef @ global.DefDef(mods, name, tparams, vparamss, tpt, rhs) if (defdef.name.endsWith("fact2")) =>
          val ident = global.Ident(typeOf[Any].typeSymbol)

          treeCopy.DefDef(defdef, mods, name, tparams, vparamss, ident, rhs)
        case t => super.transform(t)
      }
    }

  }
}
