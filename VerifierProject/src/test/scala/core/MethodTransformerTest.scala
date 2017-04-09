package core

import org.scalatest.{BeforeAndAfter, FunSuite}
import viper.silver.ast._

/**
  * Created by Severin on 2017-04-05.
  */
class MethodTransformerTest extends FunSuite with BeforeAndAfter {

  var transformer: MethodTransformer = _
  
  def createDummyMethod(name: String, 
                        _body: Stmt,
                        formalArgs: Seq[LocalVarDecl] = Seq(), 
                        formalReturns: Seq[LocalVarDecl] = Seq(), 
                        _pres: Seq[Exp] = Seq(), 
                        _posts: Seq[Exp] = Seq(), 
                        _locals: Seq[LocalVarDecl] = Seq()
                       ): Method = {
    Method(name, formalArgs, formalReturns, _pres, _posts, _locals, _body)()
  }

  before {
    transformer = new MethodTransformer()
  }

  test("simple assignment") {
    val initMeth: Method = createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x")(Int), IntLit(1)())()
    ))())
    val targetMeth: Method = createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x_0")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x_1")(Int), IntLit(1)())()
    ))())

    val transformedMeth: Method = transformer.transform(initMeth)

    assert(transformedMeth == targetMeth, "should have converted Method correctly")
  }

  test("assignment, var on rhs") {
    val initMeth = createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x")(Int), Add(LocalVar("x")(Int),IntLit(1)())())()
    ))())
    val targetMeth = createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x_0")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x_1")(Int), Add(LocalVar("x_0")(Int),IntLit(1)())())()
    ))())

    val transformedMeth = transformer.transform(initMeth)

    assert(transformedMeth == targetMeth, "should have converted Method correctly")
  }
}
