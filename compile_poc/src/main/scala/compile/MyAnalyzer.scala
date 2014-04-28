package compile

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Global

trait MyAnalyzer extends Analyzer {
  selfAnalyser =>
  val global: Global
  var cyclicReferences: List[global.Ident] = Nil
  class MyTyper(context: Context) extends selfAnalyser.Typer(context) {
    import global._

    override def typedCases(cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef] = {
      val typedCases = super.typedCases(cases, pattp, pt);
      if (typedCases.exists(_.isErroneous)) {
        println("Has errorneous case def !!!");

        val okTypes = typedCases.filter(!_.isErroneous).map(t => t.tpe)
        val errTypes = typedCases.filter(_.isErroneous)
        val okLub = ptOrLub(okTypes, WildcardType)
        if (errTypes.size > 1) throw new IllegalStateException("Too many errorneous cases")
        val errorCase = errTypes(0)
        println("LUB: " + okLub);
      }
      typedCases
    }

    override def typedDefDef(ddef: DefDef): DefDef = {
      val typedDef = super.typedDefDef(ddef)
      val cyclic = typedDef.exists(t => cyclicReferences.contains(t))
      if (cyclic) {
        println("Yes probably cyclic!")
        val tident = Ident(typeOf[Long].typeSymbol)
        val changedDef = treeCopy.DefDef(typedDef, typedDef.mods, typedDef.name, typedDef.tparams, typedDef.vparamss, tident, typedDef.rhs) setType NoType
        UnTyper.traverse(changedDef)
        treeBrowser.browse(changedDef)
        resetTyper
        val retyped = super.typedDefDef(changedDef)
        treeBrowser.browse(retyped)
        retyped
      } else
        typedDef
    }

    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      tree match {
        case ident @ global.Ident(s) =>
          try {
            super.typed(tree, mode, pt)
          } catch {
            case e: global.CyclicReference =>
              println("Ident cyclic error!! " + tree.id);
              cyclicReferences = ident :: cyclicReferences
              UnTyper.traverse(ident)
              ident
            //              throw e

            case e: Throwable =>
              println("Other error");
              println(e.getClass());
              throw e
          }
        case _ =>
          super.typed(tree, mode, pt)
      }
    }

  }

  override def newTyper(context: Context): Typer = {
    new MyTyper(context)
  }
}