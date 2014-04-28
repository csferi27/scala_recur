package compile

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Global

trait MyAnalyzer extends Analyzer {
  selfAnalyser =>
  val global: Global
  var cyclicReferences: List[global.Ident] = Nil
  class MyTyper(context: Context) extends selfAnalyser.Typer(context) {

    type Tree = selfAnalyser.global.Tree
    type Type = selfAnalyser.global.Type

    def printClass(tree: Tree): Unit = {
      //          println("Tree class to type: " + tree.getClass())
    }

    def checkTree(orig: Tree): Unit = {
      orig match {
        case defdef @ selfAnalyser.global.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          println(defdef);
          val b = checkNonCyclic(defdef.pos, defdef.tpe)
          println(b);
        case _ =>
      }
    }

    override def typedCase(cdef: selfAnalyser.global.CaseDef, pattpe: Type, pt: Type): selfAnalyser.global.CaseDef = {
      println((cdef, pattpe, pt))
      super.typedCase(cdef, pattpe, pt)
    }

    override def typedCases(cases: List[selfAnalyser.global.CaseDef], pattp: Type, pt: Type): List[selfAnalyser.global.CaseDef] = {
      //      val origCases = cases.map(_.duplicate)
      //      cases mapConserve { cdef =>
      //        newTyper(context.makeNewScope(cdef, context.owner)).typedCase(cdef, pattp, pt)
      //      }
      val res = super.typedCases(cases, pattp, pt);
      println("lastTreeToTyper")
      println(lastTreeToTyper)
      println(lastTreeToTyper.getClass())
      //      val zipped = res.zip(cases);
      if (res.exists(_.isErroneous)) {
        println("Has errorneous case def !!!");
        val ok = res.filter(!_.isErroneous)
        val okTypes = ok.map(t => t.tpe)
        val lub = ptOrLub(okTypes, global.WildcardType)
        val err = res.filter(_.isErroneous)

        println("Lub: " + lub);
        val errorCase = err(0)
        println(errorCase);
        println(errorCase.tpe);

        //        errorCase.

        //        res.foreach(_ match {
        //          case cd @ global.CaseDef(pat, guard, body) if (cd.tpe.isErroneous) =>
        //            println("Errorneous case def !!!");
        //            println(pat, guard, body);
        //          case _ =>
        //        })
        //        val corrected: selfAnalyser.global.CaseDef = typedCase(errorCase, lub._1, lub._1)

        //        println("Corrected");
        //        println(corrected.tpe);
        //        corrected :: ok
        //        UnTyper.traverse(errorCase)
        //        resetTyper
        //  res.foreach(cd => UnTyper.traverse(_))
        //        val r = super.typedCases(cases, lub._1, lub._1);

        //        UnTyper.traverse(errorCase)
        println(errorCase);
        println(errorCase.tpe);
        println("Retyped case defs");
        res.foreach(cd => println((cd.tpe, cd.isTyped)))
        println("--");
        //        res
        //        val r = super.typedCases(res, lub._1, lub._1);
        //        res.foreach(cd => println((cd.tpe, cd.isTyped, cd.body)))
        //        println("--");
        res
      } else {
        res
      }
    }

    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      printClass(tree)
      tree match {
        case defdef @ global.DefDef(mods, name, tparams, vparamss, tpt, rhs) if (defdef.name.endsWith("fact2")) =>
          println("Def def found");
          try {
            val r = super.typed(tree, mode, pt)
            println("Def def END");

            val cyclic = r.exists(t => cyclicReferences.contains(t))
            if (cyclic) {
              println("Yess probably cyclic!");
              println(tpt);

            }

            // FIND CASES
            // FIND OUT LUB            
            // UnTyper.traverse(defdef)
            // defdef set tpt to LUB
            // retype 

            UnTyper.traverse(defdef)
            defdef.setSymbol(global.typeOf[Long].typeSymbol)
            //            val ident = global.Ident(global.typeOf[Long].typeSymbol)
            //            global.
            //            val newFos = global.DefDef(defdef, mods, name, tparams, vparamss, pt, rhs)

            super.typed(defdef, mode, global.typeOf[Long])

            //            r
            ///////////////////////////
            ///////////////////////////
            /*
             *  IT KELL LEKEZELNI!!!
             *  Lemenni kiszamitani a tipust, untype-olni es ujrafuttatni
             *  
             *  Jo lenne meg biztosra menni hogy CyclicReference tortent
             * */
            ///////////////////////////
            ///////////////////////////
          } catch {
            case e @ _ =>
              println("Def Def error")
              throw e;
          }
        case ident @ global.Ident(s) if (ident.name.endsWith("fact")) =>
          val f = super.typed(tree, mode, pt)
          println("FACT TYPED TO");
          println(f.getClass());
          println(f.tpe);
          println(f.tpe.getClass);
          f
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
        //          super.typed(tree, mode, pt)
        case _ =>
          super.typed(tree, mode, pt)

      }
    }

  }

  override def newTyper(context: Context): Typer = {
    new MyTyper(context)
  }
}