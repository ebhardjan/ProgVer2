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

  def renameLocalVarUnique(lv: sil.LocalVar): sil.LocalVar = {
    sil.LocalVar(nameGenerator.createUniqueIdentifier(lv.name))(lv.typ)
  }

  def renameLocalVarLast(lv: sil.LocalVar): sil.LocalVar = {
    sil.LocalVar(nameGenerator.getLastIdentifier(lv.name))(lv.typ)
  }

  def replaceLocalVarWithLast(exp: sil.Exp): sil.Exp = {
    val pre: PartialFunction[sil.Node, sil.Node] = {
      case n: LocalVar => renameLocalVarLast(n)
    }
    exp.transform(pre)()
  }

  def unflattenAnd(exps: Seq[sil.Exp]): sil.Exp = {
    exps.size match {
      case 0 => sil.BoolLit(true)()
      case 1 => exps.head
      case _ => exps match {
        case fst :: rest => rest.foldLeft[sil.Exp](fst)((old, next) => sil.And(old, next)())
      }
    }
  }

  def collectLocalVarsAssigned(stmt: sil.Stmt): mutable.Map[String, Int] = {
    val varOccMap = mutable.Map[String, Int]()
    stmt.visit({
      case sil.LocalVarAssign(LocalVar(name), _) =>
        val old = varOccMap.getOrElse(name, 0)
        varOccMap.put(name, old + 1)
    })
    varOccMap
  }

  // Keep the node a while loop as a placeholder, but put the transformed version in the body.
  def transformWhileLoops(method: sil.Method): sil.Method = {
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

  def transformIfStmts[A<:sil.Node](method: A): A = {
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

  def ifStmtToDSA(ifstmt: sil.If): sil.If = {
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

  def whileStmtToDSA(whilestmt: sil.While): sil.Node = {
    // simulate havocs by increasing the version of all variables assigned in the loop beforehand
    for ((name,_) <- collectLocalVarsAssigned(whilestmt.body)) {
      nameGenerator.increaseVersion(name)
    }
    sil.Seqn(Seq(
      sil.Assert(transformToDSA(unflattenAnd(whilestmt.invs)))(),
      transformToDSA(whilestmt.body)
    ))()
  }

  def transformToDSA[A<:sil.Node](node: A): A = {
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

  def transform(method: sil.Method): sil.Method = {
    val noWhile: sil.Method = transformWhileLoops(method)
    val dsa: sil.Method = transformToDSA(noWhile)
    transformIfStmts(dsa)
  }

}
