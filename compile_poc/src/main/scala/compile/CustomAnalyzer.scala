package compile

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Global
import scala.annotation.tailrec
import scala.reflect.api.Types
import sun.util.logging.resources.logging
import scala.collection.mutable

trait CustomAnalyzer extends Analyzer {
  selfAnalyser =>
  val global: Global

  import global._
  import definitions._

  var cyclicReferences: List[global.Ident] = Nil

  class CustomTyper(context: Context) extends selfAnalyser.Typer(context) {
    import global._

    override def typedDefDef(defDef: DefDef): DefDef = {

      def updateDefDefType(dd: DefDef, dtype: Type) = {
        val msym = dd.symbol.asMethod
        msym.reset(MethodType(msym.paramss.flatten, dtype))
        dd.setSymbol(msym)
        dd.tpe = dtype;
        dd.tpt.tpe = dtype;
      }

      def deleteDefDefType(dd: DefDef): Unit = {
        dd.tpe = null;
        dd.tpt.tpe = null;
      }

      def isNotNoTypeOrNothing(typ: Type): Boolean = {
        typ != NoType && typ != typeOf[Nothing] && typ != null
      }

      def collectReturnBranches(tree: Tree): List[Tree] = {
        //TODO: should be used when tree is Block
        def hasReturn(tree: Tree): Boolean = {
          tree.exists(t => t.isInstanceOf[Return])
        }

        val res = tree match {
          case iff @ If(cond: Tree, thenp: Tree, elsep: Tree) => thenp :: elsep :: Nil
          case mmatch @ Match(selector: Tree, cases: List[CaseDef]) => cases.flatMap(collectReturnBranches)
          case block @ Block(stats: List[Tree], expr: Tree) => expr :: Nil
          case defdef @ DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) => rhs :: Nil
          case label @ LabelDef(sym, params, rhs) => rhs :: Nil
          case cased @ CaseDef(pat: Tree, guard: Tree, body: Tree) => body :: Nil
          case ret @ Return(expr: Tree) => expr :: Nil
          case _ => Nil
        }

        res.filter(t => isNotNoTypeOrNothing(t.tpe))
      }

      def deduceType(tree: Tree, pt: Type): Type = {
        if (!tree.children.isEmpty) {
          val returnBranches = collectReturnBranches(tree)
          val nonErrorneousReturnBranches = returnBranches.filter(!_.exists(_.isErroneous))
          val errorneousReturnBranches = returnBranches.filter(_.exists(_.isErroneous))
          val types = nonErrorneousReturnBranches.map(t => t.tpe)
          val typesWithPt = (defDef.tpt.tpe :: pt :: types).filter(isNotNoTypeOrNothing)

          //lub of method's current type and correctly branches 
          val lub = ptOrLub(typesWithPt, NoType)._1

          //Retype Literals on return branches as they are errorneous if there is a type mismatch
          val retypedLiterals =
            errorneousReturnBranches.filter(_.isInstanceOf[Literal]).map(
              b => { UnTyper.traverse(b); typed(b); }).filter(!_.isErroneous)

          updateDefDefType(defDef, lub)

          val retypedOkMethodCalls = errorneousReturnBranches.filter(t => t.isInstanceOf[Apply] || t.isInstanceOf[Select]).map(
            b => { UnTyper.traverse(b); typed(b); }).filter(!_.exists(_.isErroneous))

          // lub updated with types of retyped errorneous method calls
          val retypedLub = ptOrLub(lub :: retypedOkMethodCalls.map(_.tpe) ::: retypedLiterals.map(_.tpe), NoType)._1
          val errorneousReturnBranchesTypes = errorneousReturnBranches.map(deduceType(_, retypedLub))

          ptOrLub((retypedLub :: errorneousReturnBranchesTypes).filter(isNotNoTypeOrNothing), NoType)._1
        } else pt
      }
      val typedDef = super.typedDefDef(defDef)
      val typedDefWithoutImpl = context.withImplicitsDisabled(super.typedDefDef(defDef))

      //Check whether default typing was succesfull, if not do retyping on tree typed with implicits disabled!!
      if (typedDef.exists(t => cyclicReferences.contains(t)) && typedDef.exists(_.isErroneous)) {
        deleteDefDefType(defDef)
        val newType = deduceType(typedDefWithoutImpl, NoType)
        val ident = Ident(newType.typeSymbol)
        UnTyper.traverse(typedDefWithoutImpl)

        val msym = defDef.symbol.asMethod
        msym.reset(MethodType(msym.paramss.flatten, newType))

        val defCopy = treeCopy.DefDef(defDef, defDef.mods, defDef.name, defDef.tparams, defDef.vparamss, ident, defDef.rhs)
        val res = super.typedDefDef(defCopy)
        //        treeBrowser.browse(res) //Show the tree
        res
      } else
        typedDef
    }

    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      val res: Tree = tree match {
        case ident @ Ident(s) =>
          try {
            super.typed(tree, mode, pt)
          } catch {
            case e: CyclicReference =>
              cyclicReferences = ident :: cyclicReferences
              UnTyper.traverse(ident)
              ident
          }

        case _ =>
          val res = super.typed(tree, mode, pt)
          res
      }
      //Hack, makes modified repl work
      reporter.reset
      res
    }

  }

  override def newTyper(context: Context): Typer = {
    new CustomTyper(context)
  }

}
