package core

import util.DSANameGenerator
import viper.silver.ast.LocalVar
import viper.silver.{ast => sil}

import scala.collection.mutable

/**
  * Created by Severin on 2017-04-05.
  */
class MethodTransformer {
  private val nameGenerator: DSANameGenerator = new DSANameGenerator()

  /** Create a new LocalVar node with a new unique identifier
    */
  private def renameLocalVarUnique(lv: sil.LocalVar): sil.LocalVar = {
    sil.LocalVar(nameGenerator.createUniqueIdentifier(lv.name))(lv.typ)
  }

  /** Create a new LocalVar node renamed to use the last written version of DSA
    */
  private def renameLocalVarLast(lv: sil.LocalVar): sil.LocalVar = {
    sil.LocalVar(nameGenerator.getLastIdentifier(lv.name))(lv.typ)
  }

  /** Replace every local variable in the Exp with a new one, renamed to use the last written version of DSA
    */
  private def replaceLocalVarWithLast(exp: sil.Exp): sil.Exp = {
    val pre: PartialFunction[sil.Node, sil.Node] = {
      case n: LocalVar => renameLocalVarLast(n)
    }
    exp.transform(pre)()
  }

  /** Take a sequence of Exps and return an AST representing the conjunction of all the Exps
    */
  def unflattenAnd(exps: Seq[sil.Exp]): sil.Exp = {
    exps.size match {
      case 0 => sil.BoolLit(true)()
      case 1 => exps.head
      case _ => exps match {
        case fst :: rest => rest.foldLeft[sil.Exp](fst)((old, next) => sil.And(old, next)())
      }
    }
  }

  /** Collect all the local variables which are assigned to within the Stmt.
    * Returns a mapping from variable name to the number of times it has been assigned in the Stmt.
    */
  private def collectLocalVarsAssigned(stmt: sil.Stmt): mutable.Map[String, Int] = {
    val varOccMap = mutable.Map[String, Int]()
    stmt.visit({
      case sil.LocalVarAssign(LocalVar(name), _) =>
        val old = varOccMap.getOrElse(name, 0)
        varOccMap.put(name, old + 1)
    })
    varOccMap
  }

  /** Collect all the local variables which have been versioned in the DSA process.
    * @param originals The local vars as they were in the original program.
    * @return A list of all newly created variables.
    */
  private def collectNewLocalVars(originals: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = {
    val varVerMap = nameGenerator.variableMapSnapshot()
    (for (sil.LocalVarDecl(varName, typ) <- originals if varVerMap.isDefinedAt(varName)) yield {
      for (i <- 0 to varVerMap.getOrElse(varName, 0)) yield
        sil.LocalVarDecl(nameGenerator.makeIdentifier(varName, i), typ)()
    }).flatten
  }

  /** Do the transformation of while loops into a [[sil.NonDeterministicChoice]].
    * Keep the node a while loop as a placeholder, but put the transformed version in the body.
    */
  private def transformWhileLoops(method: sil.Method): sil.Method = {
    val pre: PartialFunction[sil.Node, sil.Node] = {
      case sil.While(cond, invs, locals, body) =>
        val invariant: sil.Exp = unflattenAnd(invs)
        sil.While(
          sil.BoolLit(true)(),
          invs,
          locals,
          sil.NonDeterministicChoice(
            sil.Seqn(Seq(
              sil.Inhale(sil.And(invariant, cond)())(),
              sil.Assert(invariant)(),
              body,
              sil.Inhale(sil.BoolLit(false)())()
            ))(),
            sil.Inhale(sil.And(invariant, sil.Not(cond)())())()
          )()
        )()
    }
    method.transform(pre)()
  }

  /** Transform all if statements occurring in a AST node into a [[sil.NonDeterministicChoice]]
    */
  private def transformIfStmts[A<:sil.Node](method: A): A = {
    val post: PartialFunction[sil.Node, sil.Node] = {
      case sil.If(cond, thn, els) =>
        sil.NonDeterministicChoice(
          sil.Seqn(Seq(
            sil.Inhale(cond)(),
            thn
          ))(),
          sil.Seqn(Seq(
            sil.Inhale(sil.Not(cond)())(),
            els
          ))()
        )()
    }
    method.transform()(_ => true, post)
  }

  /** Do the DSA transformation on a single If stmt.
    */
  private def ifStmtToDSA(ifstmt: sil.If): sil.If = {
    val dsaCond: sil.Exp = replaceLocalVarWithLast(ifstmt.cond)
    val assignedVarsThen: mutable.Map[String, Int] = collectLocalVarsAssigned(ifstmt.thn)
    val assignedVarsElse: mutable.Map[String, Int] = collectLocalVarsAssigned(ifstmt.els)
    val assignedInBoth: Set[String] = assignedVarsThen.keySet.intersect(assignedVarsElse.keySet).toSet
    val originalAssignments: Map[String, Int] = (for (variable <- assignedInBoth)
      yield variable -> nameGenerator.getVersion(variable)).toMap
    // replace variables in then part
    val newThen = transformToDSA(ifstmt.thn)
    // reset version numbers for common variables
    for (variable <- assignedInBoth) {
      nameGenerator.setVersion(variable, originalAssignments(variable))
    }
    // replace variables in else part
    val newElse = transformToDSA(ifstmt.els)
    // fix version numbers to reflect the maximum
    for (variable <- assignedInBoth) {
      val old = originalAssignments(variable)
      nameGenerator.setVersion(variable, math.max(old + assignedVarsThen(variable),
                                                  old + assignedVarsElse(variable)))
    }
    sil.If(dsaCond, newThen, newElse)()
  }

  /** Do the DSA transformation on a single While loop.
    * Assumes the while loop to already be in the intermediate form resulting from [[transformWhileLoops()]].
    * Return the flattened version, no more sil.While node.
    */
  private def whileStmtToDSA(whilestmt: sil.While): sil.Node = {
    // simulate havocs by increasing the version of all variables assigned in the loop beforehand
    for ((name,_) <- collectLocalVarsAssigned(whilestmt.body)) {
      nameGenerator.increaseVersion(name)
    }
    sil.Seqn(Seq(
      sil.Assert(transformToDSA(unflattenAnd(whilestmt.invs)))(),
      transformToDSA(whilestmt.body)
    ))()
  }

  /** Transform a silver AST node into DSA form.
    */
  private def transformToDSA[A<:sil.Node](node: A): A = {
    val pre: PartialFunction[sil.Node, sil.Node] = {
      case n: sil.LocalVarAssign =>
        val newRhs: sil.Exp = replaceLocalVarWithLast(n.rhs)
        sil.Inhale(sil.EqCmp(renameLocalVarUnique(n.lhs), newRhs)())()
      case n: sil.Exp => replaceLocalVarWithLast(n)
      case n: sil.If => ifStmtToDSA(n)
      case n: sil.While => whileStmtToDSA(n)
    }
    node.transform(pre)()
  }

  /** Transform a Method node. Eliminate all the while loops and if statements to non-deterministic choices,
    * use DSA form and replace assignments with 'assume' statements ([[sil.Inhale]]).
    * @param method The method to transform.
    * @return The transformed method.
    */
  def transform(method: sil.Method): sil.Method = {
    val noWhile: sil.Method = transformWhileLoops(method)
    val dsa: sil.Method = transformToDSA(noWhile)
    dsa.locals ++= collectNewLocalVars(dsa.locals)
    transformIfStmts(dsa)
  }

}
