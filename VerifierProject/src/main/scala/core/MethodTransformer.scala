package core

import util.DSANameGenerator
import viper.silver.ast.{Exists, Exp, Forall, Inhale, LocalVar}
import viper.silver.verifier.errors.{LoopInvariantNotEstablished, LoopInvariantNotPreserved}
import viper.silver.verifier.reasons.AssertionFalse
import viper.silver.{ast => sil}

import scala.collection.mutable

/**
  * Created by Severin on 2017-04-05.
  */
class MethodTransformer {
  private var nameGenerator: DSANameGenerator = _

  /** Take a sequence of Exps and return an AST representing the conjunction of all the Exps
    */
  def unflattenAnd(exps: Seq[sil.Exp]): sil.Exp = {
    exps.size match {
      case 0 => sil.BoolLit(true)()
      case 1 => exps.head
      case _ => exps.reduce[sil.Exp]((acc, next) => sil.And(acc, next)())
    }
  }

  /** Recursively visit node, flattening all the [[sil.Seqn]] nodes to not contain nested sequences.
    */
  def flattenSequences[A<:sil.Node](node: A): A = {
    val pre: PartialFunction[sil.Node, sil.Node] = {
      case sil.Seqn(ss: Seq[sil.Stmt]) => sil.Seqn(ss.flatMap({
        case n: sil.Seqn => flattenSequences(n).ss
        case n: sil.Stmt => Seq(n)
      }))()
    }
    node.transform(pre)()
  }

  /** Transform all if statements occurring in a AST node into a [[sil.NonDeterministicChoice]]
    */
  private def transformIfStmts[A<:sil.Node](node: A): A = {
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
    node.transform()(_ => true, post)
  }

  /** Transform assert statements to be assert; assume.
    */
  private def transformAssertStmts[A<:sil.Node](node: A): A = {
    val pre: PartialFunction[sil.Node, sil.Node] = {
      case n @ sil.Assert(exp) => sil.Seqn(Seq(
        n,
        sil.Inhale(exp)()
      ))()
    }
    node.transform(pre)()
  }

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
  private def replaceLocalVarWithLast(exp: sil.Exp, exceptions: Seq[String] = Seq()): sil.Exp = {
    exp match {
      case f @ sil.Forall(variables, triggers: Seq[sil.Trigger], e) =>
        val excepts: Seq[String] = variables.map(d => d.name)
        val newTriggers: Seq[sil.Trigger] = triggers.map(t =>
          sil.Trigger(t.exps.map(trigE => replaceLocalVarWithLast(trigE, excepts)))(t.pos, t.info))
        sil.Forall(variables,
          newTriggers,
          replaceLocalVarWithLast(e, excepts))(f.pos, f.info)
      case ex @ sil.Exists(variables, e) =>
        val excepts: Seq[String] = variables.map(d => d.name)
        sil.Exists(variables, replaceLocalVarWithLast(e, excepts))(ex.pos, ex.info)
      case _ =>
        val pre: PartialFunction[sil.Node, sil.Node] = {
          case n: sil.LocalVar =>
            if (exceptions.contains(n.name)) {
              n
            } else {
              renameLocalVarLast(n)
            }
        }
        exp.transform(pre)()
    }
  }

  /** Add all declared variables in method to the name generator.
    */
  private def addDeclaredVarsToNameGenerator(method: sil.Method): Seq[sil.LocalVarDecl] = {
    addUnversionedVarsToNameGenerator(method.formalArgs)
    addDeclaredVarsToNameGenerator(method.formalReturns)
    addDeclaredVarsToNameGenerator(method.locals)
    val bodyDecls = collectLocalVarsDeclared(method.body).toSeq
    addDeclaredVarsToNameGenerator(bodyDecls)

    method.formalArgs ++ method.formalReturns ++ method.locals ++ bodyDecls
  }

  /** Add all variable names in the input Seq to the name generator as unversioned variables.
    */
  private def addUnversionedVarsToNameGenerator(varDecls: Seq[sil.LocalVarDecl]): Unit = {
    for (varDecl <- varDecls) {
      nameGenerator.putUnversionedVariable(varDecl.name)
    }
  }

  /** Add all variable names in the input Seq to the name generator.
    */
  private def addDeclaredVarsToNameGenerator(varDecls: Seq[sil.LocalVarDecl]): Unit = {
    for (varDecl <- varDecls) {
      if (nameGenerator.getVersion(varDecl.name) == -1) nameGenerator.createUniqueIdentifier(varDecl.name)
    }
  }

  /** Collect all variable declarations in the statement to a set.
    */
  private def collectLocalVarsDeclared(stmt: sil.Stmt): Set[sil.LocalVarDecl] = {
    val declSet: mutable.Set[sil.LocalVarDecl] = mutable.Set()
    stmt.visit({
      case n: sil.LocalVarDecl => declSet.add(n)
    })
    declSet.toSet
  }

  /** Collect all the local variables which are assigned to within the Stmt.
    * Returns a mapping from variable name to the number of times it has been assigned in the Stmt.
    */
  private def collectLocalVarsAssigned(stmt: sil.Stmt): mutable.Map[sil.LocalVar, Int] = {
    val varOccMap = mutable.Map[sil.LocalVar, Int]()
    stmt.visit({
      case sil.LocalVarAssign(lv, _) =>
        val old = varOccMap.getOrElse(lv, 0)
        varOccMap.put(lv, old + 1)
    })
    varOccMap
  }

  /** Collect all the local variables which have been versioned in the DSA process.
    * @param originals The local vars as they were in the original program.
    * @return A list of all newly created variables.
    */
  private def collectNewLocalVars(originals: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = {
    val varVerMap: Map[String, Int] = nameGenerator.variableMapSnapshot(useMaxCounts =  true)
    (for (sil.LocalVarDecl(varName, typ) <- originals if varVerMap.isDefinedAt(varName)) yield {
      for (i <- 0 to varVerMap.getOrElse(varName, 0)) yield
        sil.LocalVarDecl(nameGenerator.makeIdentifier(varName, i), typ)()
    }).flatten
  }

  private def setVariableVersionsToMax(assignedVarsFirst: mutable.Map[LocalVar, Int],
                                       assignedVarsSecond: mutable.Map[LocalVar, Int],
                                       originalAssignments: Map[String, Int]) = {
    val assignedInEither: Set[LocalVar] = assignedVarsFirst.keySet.union(assignedVarsSecond.keySet).toSet
    for (variable <- assignedInEither) {
      val old = originalAssignments(variable.name)
      nameGenerator.setVersion(variable.name,
        math.max(old + assignedVarsFirst.getOrElse(variable, 0), old + assignedVarsSecond.getOrElse(variable, 0)))
    }
  }

  /** Do the DSA transformation on a single If stmt.
    */
  private def ifStmtToDSA(ifstmt: sil.If): sil.If = {
    val dsaCond: Exp = replaceLocalVarWithLast(ifstmt.cond)
    val assignedVarsThen: mutable.Map[LocalVar, Int] = collectLocalVarsAssigned(ifstmt.thn)
    val assignedVarsElse: mutable.Map[LocalVar, Int] = collectLocalVarsAssigned(ifstmt.els)
    val originalAssignments: Map[String, Int] =
      nameGenerator.variableMapSnapshot(
        (assignedVarsThen.keySet ++ assignedVarsElse.keySet).map(lv => lv.name).toSet)
    // replace variables in then part
    val newThen = transformToDSA(ifstmt.thn)
    // reset version numbers for common variables
    nameGenerator.bulkUpdateVersions(originalAssignments)
    // replace variables in else part
    val newElse = transformToDSA(ifstmt.els)
    // fix version numbers to reflect the maximum
    setVariableVersionsToMax(assignedVarsThen, assignedVarsElse, originalAssignments)
    // add assignment statements where needed as the number of assigned versions per branch might differ
    var additionalThenAssigns: Seq[Inhale] = Seq()
    var additionalElseAssigns: Seq[Inhale] = Seq()

    def additionalAssert(variable: LocalVar, old: Int, first: Int, second: Int): Inhale = {
      sil.Inhale(
        sil.EqCmp(
          sil.LocalVar(nameGenerator.makeIdentifier(variable.name, old + first))(variable.typ),
          LocalVar(nameGenerator.makeIdentifier(variable.name, old + second))(variable.typ))()
      )()
    }
    for (variable <- assignedVarsThen.keys ++ assignedVarsElse.keys) {
      val old = originalAssignments(variable.name)
      val thenN = assignedVarsThen.getOrElse(variable, 0)
      val elseN = assignedVarsElse.getOrElse(variable, 0)
      if (thenN > elseN) {
        additionalElseAssigns = additionalAssert(variable, old, thenN, elseN) +: additionalElseAssigns
      } else if (thenN < elseN) {
        additionalThenAssigns = additionalAssert(variable, old, elseN, thenN) +: additionalThenAssigns
      }
    }
    sil.If(dsaCond,
      sil.Seqn(newThen +: additionalThenAssigns)(),
      sil.Seqn(newElse +: additionalElseAssigns)()
    )()
  }

  /** Do the transformation on a [[sil.While]] loop.
    */
  private def transformWhileStmt(whilestmt: sil.While): sil.Node = {
    val dsaInvariantAssertsBefore: Seq[sil.Assert] = whilestmt.invs
      .map(i => transformToDSA(i))
      .map(i => sil.Assert(i)(i.pos, createInvariantNotEstablishedError(i)))

    // simulate havocs by increasing the version of all variables assigned in the loop beforehand
    val varsAssignedInBody: Set[String] = collectLocalVarsAssigned(whilestmt.body).keys
      .map(lv => lv.name).toSet -- whilestmt.locals.map(lvd => lvd.name)
    nameGenerator.increaseVersion(varsAssignedInBody)
    val varVersionsAfterHavoc: Map[String, Int] = nameGenerator.variableMapSnapshot(varsAssignedInBody)

    // transform invariants, condition and body to DSA, using havoced versions
    val dsaInvariants: Seq[sil.Exp] = whilestmt.invs.map(i => transformToDSA(i))
    val dsaCond: sil.Exp = transformToDSA(whilestmt.cond)

    val dsaBody: sil.Stmt = transformToDSA(whilestmt.body)
    // transform invariants again to use latest versions to verify invariants after the body
    val dsaInvariantsAssertsAfter: Seq[sil.Assert] = whilestmt.invs
      .map(i => transformToDSA(i))
      .map(i => sil.Assert(i)(i.pos, createInvariantNotPreservedError(i)))

    val result = sil.Seqn(dsaInvariantAssertsBefore ++
      Seq(
        sil.NonDeterministicChoice(
          sil.Seqn(
            Seq(sil.Inhale(sil.And(unflattenAnd(dsaInvariants), dsaCond)())(),
              dsaBody)
              ++ dsaInvariantsAssertsAfter
              ++ Seq(sil.Inhale(sil.BoolLit(false)())())
          )(),
          sil.Inhale(sil.And(unflattenAnd(dsaInvariants), sil.Not(dsaCond)())())()
        )()
      ))()
    // reset variables which were assigned in the loop to their versions after the havocs,
    // because the traces executing the body don't survive
    nameGenerator.bulkUpdateVersions(varVersionsAfterHavoc)
    result
  }

  private def createInvariantNotEstablishedError(i: Exp): CustomError = {
    val originalExpression = i.info.asInstanceOf[CustomError].expr
    val error = LoopInvariantNotEstablished(originalExpression, AssertionFalse(originalExpression))
    CustomError(error, originalExpression)
  }

  private def createInvariantNotPreservedError(i: Exp): CustomError = {
    val originalExpression = i.info.asInstanceOf[CustomError].expr
    val error = LoopInvariantNotPreserved(originalExpression, AssertionFalse(originalExpression))
    CustomError(error, originalExpression)
  }


  /** Take a method and put the preconditions into the body as assume statements.
    */
  private def addPreConditions(method: sil.Method): sil.Method = {
    val preconds: Seq[sil.Exp] = for (cond <- method.pres) yield replaceLocalVarWithLast(cond)
    val result = method
    result.body = sil.Seqn(preconds.map(exp => sil.Inhale(exp)()) :+ method.body)()
    result
  }

  /** Take a method and put the postconditions into the body as assert statements.
    */
  private def addPostConditions(method: sil.Method): sil.Method = {
    val postconds: Seq[sil.Exp] = for (cond <- method.posts) yield replaceLocalVarWithLast(cond)
    val result = method
    result.body = sil.Seqn(method.body +: postconds.map(exp => sil.Assert(exp)(exp.pos, exp.info)))()
    result
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
      case n: sil.While => transformWhileStmt(n)
    }
    node.transform(pre)()
  }

  /** Transform a Method node. Eliminate all the while loops and if statements to non-deterministic choices,
    * use DSA form and replace assignments with 'assume' statements ([[sil.Inhale]]).
    * @param method The method to transform.
    * @return The transformed method.
    */
  def transform(method: sil.Method): sil.Method = {
    nameGenerator = new DSANameGenerator()
    val declarations: Seq[sil.LocalVarDecl] = addDeclaredVarsToNameGenerator(method)
    var intermediate = method
    intermediate.body = transformToDSA(method.body)
    intermediate.locals = collectNewLocalVars(declarations)
    intermediate = transformIfStmts(intermediate)
    intermediate = transformAssertStmts(intermediate)
    intermediate = addPreConditions(intermediate)
    intermediate = addPostConditions(intermediate)
    flattenSequences(intermediate)
  }

}
