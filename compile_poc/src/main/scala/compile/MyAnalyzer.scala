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
      println("Def def BEGIN");
      val typedDef = super.typedDefDef(ddef)
      println("Def def END");
      val cyclic = typedDef.exists(t => cyclicReferences.contains(t))
      if (cyclic) {
        println("Yes probably cyclic!")
        UnTyper.traverse(typedDef)
      }
      typedDef
    }

    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      tree match {
        //        case ident @ global.Ident(s) if (ident.name.endsWith("fact")) =>
        //          val f = super.typed(tree, mode, pt)
        //          println("FACT TYPED TO");
        //          println(f.getClass());
        //          println(f.tpe);
        //          println(f.tpe.getClass);
        //          f
        case ident @ global.Ident(s) =>
          try {
            super.typed(tree, mode, pt)
          } catch {
            case e: global.CyclicReference =>
              println("Ident cyclic error!! " + tree.id);
              cyclicReferences = ident :: cyclicReferences
              throw e

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