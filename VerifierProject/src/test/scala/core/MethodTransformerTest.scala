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
      Inhale(EqCmp(LocalVar("x_0")(Int), IntLit(0)())())(),
      Inhale(EqCmp(LocalVar("x_1")(Int), IntLit(1)())())()
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
      Inhale(EqCmp(LocalVar("x_0")(Int), IntLit(0)())())(),
      Inhale(EqCmp(LocalVar("x_1")(Int), Add(LocalVar("x_0")(Int),IntLit(1)())())())()
    ))())

    val transformedMeth = transformer.transform(initMeth)

    assert(transformedMeth == targetMeth, "should have converted Method correctly")
  }

  test("if statement simple") {
    val initMeth = createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x")(Int), IntLit(0)())(),
      If(EqCmp(LocalVar("x")(Int), IntLit(0)())(),
        Seqn(Seq(
          LocalVarAssign(LocalVar("x")(Int), IntLit(1)())()
        ))(),
        Seqn(Seq(
          LocalVarAssign(LocalVar("x")(Int), IntLit(2)())()
        ))()
      )(),
      Assert(EqCmp(LocalVar("x")(Int), IntLit(1)())())()
    ))())
    val targetMeth = createDummyMethod("foo", Seqn(Seq(
      Inhale(EqCmp(LocalVar("x_0")(Int), IntLit(0)())())(),
      NonDeterministicChoice(
        Seqn(Seq(
          Inhale(EqCmp(LocalVar("x_0")(Int), IntLit(0)())())(),
          Inhale(EqCmp(LocalVar("x_1")(Int), IntLit(1)())())()
        ))(),
        Seqn(Seq(
          Inhale(Not(EqCmp(LocalVar("x_0")(Int), IntLit(0)())())())(),
          Inhale(EqCmp(LocalVar("x_1")(Int), IntLit(2)())())()
        ))()
      )(),
      Assert(EqCmp(LocalVar("x_1")(Int), IntLit(1)())())(),
      Inhale(EqCmp(LocalVar("x_1")(Int), IntLit(1)())())()
    ))())

    val transformedMeth = transformer.transform(initMeth)

    assert(transformedMeth == targetMeth, "should have converted if stmt correctly")
  }

  test("while loop simple") {
    /*
    method foo()
      invariant x >= 0
    {
      var x: Int = 0
      while (x > 0) {
        x = x - 1
      }
      assert x == 0
    }
     */
    val xGt0: Exp = GtCmp(LocalVar("x")(Int), IntLit(0)())()
    val dsaXGt0: Exp = GtCmp(LocalVar("x_1")(Int), IntLit(0)())()
    val inv: Seq[Exp] = Seq(GeCmp(LocalVar("x")(Int), IntLit(0)())())
    val dsaInv: Seq[Exp] = Seq(GeCmp(LocalVar("x_0")(Int), IntLit(0)())())
    val tInv: Seq[Exp] = Seq(GeCmp(LocalVar("x_1")(Int), IntLit(0)())())
    val initMeth = createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x")(Int), IntLit(5)())(),
      While(xGt0,
        inv,
        Seq(LocalVarDecl("x", Int)()),
        LocalVarAssign(LocalVar("x")(Int), Sub(LocalVar("x")(Int), IntLit(1)())())()
      )(),
      Assert(EqCmp(LocalVar("x")(Int), IntLit(0)())())()
    ))())

    val targetMeth = createDummyMethod("foo", Seqn(Seq(
      Inhale(EqCmp(LocalVar("x_0")(Int), IntLit(5)())())(),
      Assert(dsaInv.head)(),
      Inhale(dsaInv.head)(),
      NonDeterministicChoice(
        Seqn(Seq(
          Inhale(And(tInv.head, dsaXGt0)())(),
          Inhale(EqCmp(LocalVar("x_2")(Int), Sub(LocalVar("x_1")(Int), IntLit(1)())())())(),
          Assert(tInv.head)(),
          Inhale(tInv.head)(),
          Inhale(BoolLit(false)())()
        ))(),
        Inhale(And(tInv.head, Not(dsaXGt0)())())()
      )(),
      Assert(EqCmp(LocalVar("x_1")(Int), IntLit(0)())())(),
      Inhale(EqCmp(LocalVar("x_1")(Int), IntLit(0)())())()
    ))())

    val transformedMeth = transformer.transform(initMeth)

    assert(transformedMeth == targetMeth, "should have converted while loop correctly")
  }
}
