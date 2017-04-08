package core

import smtlib.parser.Terms._
import smtlib.theories.Core
import smtlib.theories.Core.Equals
import smtlib.theories.Ints.NumeralLit
import viper.silver.ast._

/**
  * Created by jan on 07.04.17.
  */
object WlpStar {

  def wlpStar(stmt: Stmt, delta: Set[VerificationCondition]): Set[VerificationCondition] = {
    stmt match {
      case s@Seqn(ss) =>
        val ssFiltered = ss.filter(s => !s.isInstanceOf[Seqn])
        if (ssFiltered.size == 1) {
          wlpStar(ssFiltered.head, delta)
        } else {
          wlpStar(ssFiltered.head, wlpStar(Seqn(ssFiltered.tail)(s.pos, s.info), delta))
        }
      case a@Assert(e) =>
        delta + VerificationCondition(a, e, toTerm(e))
      case Inhale(exp) =>
        val a1 = toTerm(exp)
        delta.map(a => VerificationCondition(a.assert, a.exp, Core.Implies(a1, a.formula)))
      case NonDeterministicChoice(s1, s2) =>
        wlpStar(s1, delta) ++ wlpStar(s2, delta)
    }
  }

  private def toTerm(exp: Exp): Term = {
    exp match {
      case EqCmp(left, right) => Equals(toTerm(left), toTerm(right))
      case LocalVar(name) => QualifiedIdentifier(SimpleIdentifier(SSymbol(name)))
      case IntLit(i) => NumeralLit(i)
    }
  }

}

case class VerificationCondition(var assert: Assert, var exp: Exp, var formula: Term) {
  override def toString: String = {
    f"VerCond($assert, $exp, $formula)"
  }
}
