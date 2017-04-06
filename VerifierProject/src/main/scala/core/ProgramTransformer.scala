package core

import util.DSANameGenerator
import viper.silver.ast.LocalVar
import viper.silver.{ast => sil}

/**
  * Created by Severin on 2017-04-05.
  */
class ProgramTransformer {
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

  def transformToDSA(program: sil.Program): sil.Program = {
    val pre: PartialFunction[sil.Node, sil.Node] = {
      case n: sil.LocalVarAssign => {
        val newRhs: sil.Exp = replaceLocalVarWithLast(n.rhs)
        sil.LocalVarAssign(renameLocalVarUnique(n.lhs), newRhs)()
      }
      case n: sil.Exp => replaceLocalVarWithLast(n)
    }
    val post: PartialFunction[sil.Node, sil.Node] = {
      case n => n;
    }
    program.transform(pre)(n => !pre.isDefinedAt(n), post)
  }

}
