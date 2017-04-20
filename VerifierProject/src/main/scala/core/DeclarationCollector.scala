package core

import smtlib.parser.Commands._
import smtlib.parser.Terms._
import util.ViperToSmtlibUtils
import viper.silver.ast.{Bool, Domain, DomainFunc, DomainType, Int, LocalVarDecl, Program, Type}

/**
  * Created by jan on 08.04.17.
  */
object DeclarationCollector {

  def collectDeclarations(program: Program, locals: Seq[LocalVarDecl]): Seq[Command] = {
    collectDatatypeDeclarations(program.domains) ++
      collectMethodLocalDeclarations(locals)
  }

  private def collectDatatypeDeclarations(domains: Seq[Domain]): Seq[Command] = {
    // TODO what exactly else do we need to support here? -> check what viper can do and the project descr.
    var commands = Seq[Command]()
    commands = commands :+
      DeclareDatatypes(domains.map(d =>
        (SSymbol(d.name), Seq(Constructor(SSymbol(d.name), Seq())))
      ))
    domains.foreach(d => commands = commands ++ collectFunctions(d.functions))
    commands
  }

  private def collectFunctions(functions: Seq[DomainFunc]): Seq[Command] = {
    functions.map(f =>
      DeclareFun(
        SSymbol(f.name),
        f.formalArgs.map(a => ViperToSmtlibUtils.toSort(a.typ)),
        ViperToSmtlibUtils.toSort(f.typ)
      )
    )
  }

  private def collectMethodLocalDeclarations(locals: Seq[LocalVarDecl]): Seq[Command] = {
    locals.map(l => DeclareConst(SSymbol(l.name), ViperToSmtlibUtils.toSort(l.typ)))
  }

  def collectAxioms(domains: Seq[Domain]): Seq[Term] = {
    // in case there are no axioms we just use (and true true)
    var axioms = Seq[Term]()
    domains.foreach(d => d.axioms.foreach(a => {
      axioms = axioms :+ ViperToSmtlibUtils.toTerm(a.exp)
    }))
    axioms
  }

}
