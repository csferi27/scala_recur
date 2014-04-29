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
        val newType = typeOf[Long]
        val ident = Ident(newType.typeSymbol)
        UnTyper.traverse(typedDef)

        val msym = ddef.symbol.asMethod
        msym.reset(MethodType(msym.paramss.flatten, newType))

        val defCopy = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, ident, ddef.rhs)
        //        defCopy.defineType(typeOf[Long])
        treeBrowser.browse(defCopy)
        val res = super.typedDefDef(defCopy)
        //        res.defineType(typeOf[Long])
        treeBrowser.browse(res)
        res
        //        typedDef
        //        defCopy
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
              cyclicReferences = ident :: cyclicReferences
              throw e

            case e: Throwable =>
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