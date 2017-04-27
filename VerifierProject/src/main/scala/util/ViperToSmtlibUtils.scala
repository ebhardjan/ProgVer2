package util

import smtlib.parser.Terms
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.theories.{Core, Ints}
import viper.silver.ast
import viper.silver.ast._

/**
  * Created by jan on 08.04.17.
  */
object ViperToSmtlibUtils {

  /**
    * Converts a given silver type into an smtlib sort
    *
    * @param typ the silver type
    * @return the smtlib sort
    */
  def toSort(typ: Type): Sort = {
    typ match {
      case Int => Sort(SimpleIdentifier(SSymbol("Int")))
      case Bool => Sort(SimpleIdentifier(SSymbol("Bool")))
      case dT: DomainType => Sort(SimpleIdentifier(SSymbol(prefixSort(dT.domainName))))
    }
  }

  def prefixSort(name: String): String = {
    "myT_" + name
  }

  /**
    * converts a given silver expression into a smt-lib term
    *
    * note: the method is "formatted" in this ugly way (new lines after the case statements) so that we can collect
    * line coverage information while running the tests and make sure that our test cover all possible expressions.
    *
    * @param exp a silver expression
    * @return a smt-lib term
    */
  def toTerm(exp: Exp): Term = {
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

      case f@ast.Forall(vars, triggers, body) =>
        Terms.Forall(sortedVar(vars.head),
          vars.tail.map(v => sortedVar(v)), addTriggers(toTerm(body), triggers, f.autoTrigger.triggers))
      case ast.Exists(vars, body) =>
        Terms.Exists(sortedVar(vars.head), vars.tail.map(v => sortedVar(v)), toTerm(body))

      case ast.DomainFuncApp(name, args, _) =>
        if (args.isEmpty) {
          qualifiedIdentifier(prefixFuncName(name))
        } else {
          Terms.FunctionApplication(qualifiedIdentifier(prefixFuncName(name)), args.map(a => toTerm(a)))
        }

      case ast.CondExp(cond, thn, els) =>
        ITE(toTerm(cond), toTerm(thn), toTerm(els))
    }
  }

  def prefixFuncName(name: String): String = {
    "my_" + name
  }

  def sortedVar(localVarDecl: LocalVarDecl): SortedVar = {
    SortedVar(SSymbol(localVarDecl.name), toSort(localVarDecl.typ))
  }

  private def addTriggers(term: Term, triggers: Seq[Trigger], autoTriggers: Seq[Trigger]): Term = {
    if (triggers.nonEmpty) {
      Terms.AnnotatedTerm(term,
        triggerToAttribute(triggers.head),
        triggers.tail.map(t => triggerToAttribute(t))
      )
    } else if (autoTriggers.nonEmpty) {
      Terms.AnnotatedTerm(term,
        triggerToAttribute(autoTriggers.head),
        autoTriggers.tail.map(t => triggerToAttribute(t))
      )
    } else {
      term
    }
  }

  private def triggerToAttribute(trigger: Trigger): Attribute = {
    Attribute(
      SKeyword("pattern"),
      Option(SList(trigger.exps.map(e => toTerm(e).asInstanceOf[SExpr]).toList))
    )
  }

  private def qualifiedIdentifier(name: String): QualifiedIdentifier = {
    QualifiedIdentifier(SimpleIdentifier(SSymbol(name)))
  }

}
