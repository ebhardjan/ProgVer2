package core

import viper.silver.ast._
import viper.silver.verifier.AbstractVerificationError
import viper.silver.verifier.errors.{AssertFailed, PostconditionViolated}
import viper.silver.verifier.reasons.AssertionFalse

/**
  * Created by jan on 21.04.17.
  */
object ErrorCreator {

  def addInfoToNodes(method: Method): Method = {
    val bodyWithInfo = addInfoToNodes(method.body)
    Method(method.name,
      method.formalArgs,
      method.formalReturns,
      method.pres,
      addInfoToPostConditions(method.posts, method),
      method.locals,
      bodyWithInfo)(method.pos, method.info)
  }

  private def addInfoToPostConditions(postConditions: Seq[Exp], method: Method): Seq[Exp] = {
    postConditions.map(p => addInfoToExpr(p, CustomError(PostconditionViolated(p, method, AssertionFalse(p)), p)))
  }

  private def addInfoToNodes(stmt: Stmt): Stmt = {
    stmt match {
      case s@Seqn(ss) =>
        Seqn(ss.map(s => addInfoToNodes(s)))(s.pos, s.info)
      case a@Assert(e) =>
        Assert(e)(a.pos, CustomError(AssertFailed(a, AssertionFalse(e)), e))
      case i@If(cond, thn, els) =>
        If(cond, addInfoToNodes(thn), addInfoToNodes(els))(i.pos, i.info)
      case w@While(cond, invs, locals, body) =>
        // We will set the kind of error in the MethodTransformer. At this point we don't know yet whether it's an
        // InvariantNotPreserved or InvariantNotEstablished error.
        val newInvariants = invs.map(i => addInfoToExpr(i, CustomError(null, i)))
        While(cond, newInvariants, locals, addInfoToNodes(body))(w.pos, w.info)
      case _ => stmt
    }
  }

  private def addInfoToExpr(expr: Exp, info: CustomError): Exp = {
    // TODO there is probably a fancier scalaier way to do this...
    expr match {
      case IntLit(i) =>
        IntLit(i)(expr.pos, info)
      case BoolLit(b) =>
        BoolLit(b)(expr.pos, info)
      case l@LocalVar(name) =>
        LocalVar(name)(l.typ, expr.pos, info)
      case CondExp(cond, thn, els) =>
        CondExp(cond, thn, els)(expr.pos, info)
      case Exists(vars, body) =>
        Exists(vars, body)(expr.pos, info)
      case Forall(vars, triggers, body) =>
        Forall(vars, triggers, body)(expr.pos, info)

      case dfa@DomainFuncApp(fname, args, typeVariables) =>
        DomainFuncApp(fname, args, typeVariables)(dfa.pos, info, dfa.typ, dfa.formalArgs, dfa.domainName)

      // Boolean operators
      case Not(e) =>
        Not(e)(expr.pos, info)
      case Or(l, r) =>
        Or(l, r)(expr.pos, info)
      case And(l, r) =>
        And(l, r)(expr.pos, info)
      case Implies(l, r) =>
        Implies(l, r)(expr.pos, info)

      // Int operations
      case Minus(e) =>
        Minus(e)(expr.pos, info)
      case Add(l, r) =>
        Add(l, r)(expr.pos, info)
      case Sub(l, r) =>
        Sub(l, r)(expr.pos, info)
      case Mul(l, r) =>
        Mul(l, r)(expr.pos, info)
      case Div(l, r) =>
        Div(l, r)(expr.pos, info)
      case Mod(l, r) =>
        Mod(l, r)(expr.pos, info)

      // Int inequalities
      case LtCmp(l, r) =>
        LtCmp(l, r)(expr.pos, info)
      case LeCmp(l, r) =>
        LeCmp(l, r)(expr.pos, info)
      case GtCmp(l, r) =>
        GtCmp(l, r)(expr.pos, info)
      case GeCmp(l, r) =>
        GeCmp(l, r)(expr.pos, info)

      // equal/unequal
      case EqCmp(l, r) =>
        EqCmp(l, r)(expr.pos, info)
      case NeCmp(l, r) =>
        NeCmp(l, r)(expr.pos, info)
    }
  }

}

case class CustomError(error: AbstractVerificationError, expr: Exp) extends Info {

  override def comment: Seq[String] = Seq("we don't use this!")

}
