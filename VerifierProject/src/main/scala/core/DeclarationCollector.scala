package core

import smtlib.parser.Terms._
import viper.silver.ast._

/**
  * Created by jan on 08.04.17.
  */
object DeclarationCollector {


  def collectDeclarations(locals: Seq[LocalVarDecl]): Map[String, Sort] = {
    locals.map(l => l.name -> toSort(l.typ)).toMap
  }

  def toSort(typ: Type): Sort = {
    typ match {
      case Int => Sort(SimpleIdentifier(SSymbol("Int")))
      case Bool => Sort(SimpleIdentifier(SSymbol("Bool")))
    }

  }

}
