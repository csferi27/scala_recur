package compile

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

class FactorialTyper(val global: Global) extends Plugin {
  import global._

  val name = "factorialtyper"
  val description = "change fact2 type o Long"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {

    val global: FactorialTyper.this.global.type = FactorialTyper.this.global
    override val runsAfter = List[String]("typer");
    override val runsRightAfter = Some[String]("typer");
    val phaseName = FactorialTyper.this.name

    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new StdPhase(prev) {
      def apply(unit: global.CompilationUnit) {
        newTransformer(unit).transformUnit(unit)
      }
    }

    def newTransformer(unit: CompilationUnit) = new global.Transformer {

      override def transform(tree: global.Tree) = tree match {
        case defdef @ global.DefDef(mods, name, tparams, vparamss, tpt, rhs) if (defdef.name.endsWith("fact2")) =>

          defdef.children.foreach(t => t match {
            case m @ global.Match(tree, defs) =>
              val types = defs.map(t => t.tpe).filter(_ != typeOf[Any])

              println(types);
              println("Types: " + types)
              println("Numeric lub: " + global.numericLub(types))
              println("Numeric glb: " + global.numericGlb(types))
              println("Lub: " + global.lub(types))
            case _ =>
          })

          defdef
        case t => super.transform(t)
      }
    }

  }
}
