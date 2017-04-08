package core

import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.{Core, Ints}
import smtlib.theories.Ints._
import viper.silver.ast._
import viper.silver.ast

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

  /**
    * converts a given silver expression into a smt-lib term
    *
    * note: the method is "formatted" in this ugly way (new lines after the case statements) so that we can collect
    * collect line coverage information while running the tests and make sure that our test cover all possible
    * expressions.
    *
    * @param exp a silver expression
    * @return a smt-lib term
    */
  private def toTerm(exp: Exp): Term = {
    exp match {
      case ast.Add(left, right) =>
        Ints.Add(toTerm(left), toTerm(right))
      case ast.Sub(left, right) =>
        Ints.Sub(toTerm(left), toTerm(right))
      case ast.Mul(left, right) =>
        Ints.Mul(toTerm(left), toTerm(right))
      case ast.Div(left, right) =>
        Ints.Div(toTerm(left), toTerm(right))
      case ast.Mod(left, right) =>
        Ints.Mod(toTerm(left), toTerm(right))

      case Minus(e) =>
        Neg(toTerm(e))

      case LtCmp(left, right) =>
        Ints.LessThan(toTerm(left), toTerm(right))
      case LeCmp(left, right) =>
        Ints.LessEquals(toTerm(left), toTerm(right))
      case GtCmp(left, right) =>
        Ints.GreaterThan(toTerm(left), toTerm(right))
      case GeCmp(left, right) =>
        Ints.GreaterEquals(toTerm(left), toTerm(right))

      case EqCmp(left, right) =>
        Equals(toTerm(left), toTerm(right))
      case NeCmp(left, right) =>
        Core.Not(Equals(toTerm(left), toTerm(right)))

      case ast.Or(left, right) =>
        Core.Or(toTerm(left), toTerm(right))
      case ast.And(left, right) =>
        Core.And(toTerm(left), toTerm(right))
      case ast.Implies(left, right) =>
        Core.Implies(toTerm(left), toTerm(right))
      case ast.Not(e) =>
        Core.Not(toTerm(e))

      case TrueLit() =>
        True()
      case FalseLit() =>
        False()

      case LocalVar(name) =>
        QualifiedIdentifier(SimpleIdentifier(SSymbol(name)))
      case IntLit(i) =>
        NumeralLit(i)
    }
  }

}

case class VerificationCondition(var assert: Assert, var exp: Exp, var formula: Term) {
  override def toString: String = {
    f"VerCond($assert, $exp, $formula)"
  }
}
