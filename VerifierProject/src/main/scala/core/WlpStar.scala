package core

import smtlib.parser.Terms._
import smtlib.theories.Core
import smtlib.theories.Core._
import util.ViperToSmtlibUtils
import viper.silver.ast._
import viper.silver.verifier.AbstractVerificationError

/**
  * Created by jan on 07.04.17.
  */
object WlpStar {

  def wlpStar(stmt: Stmt, delta: Set[VerificationCondition]): Set[VerificationCondition] = {
    stmt match {
      case s@Seqn(ss) =>
        if (ss.isEmpty) {
          Set(VerificationCondition(null, True()))
        } else if (ss.size == 1) {
          wlpStar(ss.head, delta)
        } else {
          wlpStar(ss.head, wlpStar(Seqn(ss.tail)(s.pos, s.info), delta))
        }
      case a@Assert(e) =>
        delta + VerificationCondition(a.info.asInstanceOf[CustomError].error, ViperToSmtlibUtils.toTerm(e))
      case Inhale(exp) =>
        val a1 = ViperToSmtlibUtils.toTerm(exp)
        delta.map(a => VerificationCondition(a.error, Core.Implies(a1, a.formula)))
      case NonDeterministicChoice(s1, s2) =>
        wlpStar(s1, delta) ++ wlpStar(s2, delta)
    }
  }

}

case class VerificationCondition(error: AbstractVerificationError, formula: Term) {}
