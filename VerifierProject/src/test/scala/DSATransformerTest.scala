import core.DSATransformer
import org.scalatest.{BeforeAndAfter, FunSuite}
import viper.silver.ast._

/**
  * Created by Severin on 2017-04-05.
  */
class DSATransformerTest extends FunSuite with BeforeAndAfter {

  var transformer: DSATransformer = _
  
  def createDummyProgram(domains: Seq[Domain] = Seq(), methods: Seq[Method] = Seq()): Program = {
    Program(domains, Seq(), Seq(), Seq(), methods)()
  }
  
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
    transformer = new DSATransformer()
  }

  test("simple assignment") {
    val initProg: Program = createDummyProgram(Seq(), Seq(createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x")(Int), IntLit(1)())()
    ))())))
    val targetProg: Program = createDummyProgram(Seq(), Seq(createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x_0")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x_1")(Int), IntLit(1)())()
    ))())))

    val dsaProg: Program = transformer.transformToDSA(initProg)

    assert(dsaProg == targetProg, "should have converted program correctly")
  }

  test("assignment, var on rhs") {
    val initProg = createDummyProgram(Seq(), Seq(createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x")(Int), Add(LocalVar("x")(Int),IntLit(1)())())()
    ))())))
    val targetProg = createDummyProgram(Seq(), Seq(createDummyMethod("foo", Seqn(Seq(
      LocalVarAssign(LocalVar("x_0")(Int), IntLit(0)())(),
      LocalVarAssign(LocalVar("x_1")(Int), Add(LocalVar("x_0")(Int),IntLit(1)())())()
    ))())))

    val dsaProg = transformer.transformToDSA(initProg)

    assert(dsaProg == targetProg, "should have converted program correctly")
  }
}
