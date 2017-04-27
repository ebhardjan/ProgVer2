package core

import smtlib.parser.Commands._
import smtlib.parser.Terms._
import util.ViperToSmtlibUtils
import viper.silver.ast.{Domain, DomainFunc, LocalVarDecl, Program}

/**
  * Created by jan on 08.04.17.
  */
object DeclarationCollector {

  def collectDeclarations(program: Program, locals: Seq[LocalVarDecl]): Seq[Command] = {
    collectSortDeclarations(program.domains) ++
      collectMethodLocalDeclarations(locals)
  }

  private def collectSortDeclarations(domains: Seq[Domain]): Seq[Command] = {
    domains.map(d => DeclareSort(SSymbol(ViperToSmtlibUtils.prefixSort(d.name)), 0)) ++
      domains.flatMap(d => collectFunctions(d.functions))
  }

  private def collectFunctions(functions: Seq[DomainFunc]): Seq[Command] = {
    functions.map(f =>
      DeclareFun(
        SSymbol(ViperToSmtlibUtils.prefixFuncName(f.name)),
        f.formalArgs.map(a => ViperToSmtlibUtils.toSort(a.typ)),
        ViperToSmtlibUtils.toSort(f.typ)
      )
    )
  }

  private def collectMethodLocalDeclarations(locals: Seq[LocalVarDecl]): Seq[Command] = {
    locals.map(l => DeclareConst(SSymbol(l.name), ViperToSmtlibUtils.toSort(l.typ)))
  }

  def collectAxioms(domains: Seq[Domain]): Seq[Term] = {
    domains.flatMap(d =>
      d.axioms.map(a => ViperToSmtlibUtils.toTerm(a.exp))
    )
  }

}
