package compile

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Global
import scala.annotation.tailrec
import scala.reflect.api.Types

trait MyAnalyzer extends Analyzer {
  selfAnalyser =>
  val global: Global
  var cyclicReferences: List[global.Ident] = Nil
  var retypedTrees: Map[global.Tree, global.Type] = Map()

  class MyTyper(context: Context) extends selfAnalyser.Typer(context) {
    import global._

    override def typedDefDef(ddef: DefDef): DefDef = {
      def collectReturnBranches(tree: Tree): List[Tree] = {
        //TODO: should be used when tree is Block
        def hasReturn(tree: Tree): Boolean = {
          tree.exists(t => t.isInstanceOf[Return])
        }

        val res = tree match {
          case iff @ If(cond: Tree, thenp: Tree, elsep: Tree) => thenp :: elsep :: Nil
          case mmatch @ Match(selector: Tree, cases: List[CaseDef]) => cases
          case block @ Block(stats: List[Tree], expr: Tree) => stats.last :: Nil
          case defdef @ DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) => rhs :: Nil
          case label @ LabelDef(sym, params, rhs) => rhs :: Nil
          case cased @ CaseDef(pat: Tree, guard: Tree, body: Tree) => body :: Nil

          case _ => Nil
        }

        res.filter(t => t != NoType).filter(t => t != typeOf[Nothing])
      }

      def deduceType(tree: Tree, pt: Type): Type = {
        def isNotNoTypeOrNothing(typ: Type): Boolean = {
          typ != NoType && typ != typeOf[Nothing]
        }
        if (!tree.children.isEmpty) {
          val returnBranches = collectReturnBranches(tree)
          val nonErrorneousReturnBranches = returnBranches.filter(!_.exists(_.isErroneous))
          val errorneousReturnBranches = returnBranches.filter(_.exists(_.isErroneous))
          val types = nonErrorneousReturnBranches.map(t => t.tpe)
          val typesWithPt = (pt :: types).filter(isNotNoTypeOrNothing)
          //          println("typesWithPt: " + typesWithPt);

          val lub = ptOrLub(typesWithPt, NoType)._1
          val errorTypes = errorneousReturnBranches.map(deduceType(_, lub))
          val typesWithLub = (lub :: errorTypes).filter(isNotNoTypeOrNothing)
          //          println("typesWithLub: " + typesWithLub);
          ptOrLub(typesWithLub, NoType)._1
        } else pt
      }

      val typedDef = super.typedDefDef(ddef)
      if (typedDef.exists(t => cyclicReferences.contains(t)) && typedDef.exists(_.isErroneous)) {
        //        treeBrowser.browse(typedDef) //Show the tree
        val newType = deduceType(typedDef, NoType)
        println("NEW TYPE: " + newType);
        val ident = Ident(newType.typeSymbol)
        UnTyper.traverse(typedDef)

        val msym = ddef.symbol.asMethod
        msym.reset(MethodType(msym.paramss.flatten, newType))

        val defCopy = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, ident, ddef.rhs)

        val res = super.typedDefDef(defCopy)
        //        treeBrowser.browse(res) //Show the tree
        res
      } else
        typedDef
    }

    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      tree match {
        case ident @ Ident(s) =>
          try {
            super.typed(tree, mode, pt)
          } catch {
            case e: CyclicReference =>
              println("cyclic ident");
              cyclicReferences = ident :: cyclicReferences
              UnTyper.traverse(ident)
              //              context.errBuffer.clear
              ident

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
