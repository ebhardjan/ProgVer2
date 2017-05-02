package core

import java.io.{BufferedOutputStream, File, FileOutputStream}

import smtlib.parser.Commands._
import smtlib.parser.CommandsResponses._
import smtlib.parser.Terms.Term
import smtlib.theories.Core._
import viper.silver.frontend.SilFrontend
import viper.silver.verifier.errors._
import viper.silver.verifier.reasons._
import viper.silver.verifier.{VerificationError, VerificationResult, Failure => ViperFailure, Success => ViperSuccess}
import viper.silver.{ast => sil}

import scala.sys.process.{ProcessIO, _}


object Main extends SilFrontend{ // "Sil" is an (old) name for the Viper intermediate language
  protected var verifierInstance: MyVerifier = _ // initialised to null - this will be set in the "execute" method below (which calls createVerifier)

  def main(args: Array[String]) {
    try {
      execute(args)
      /* Will call createVerifier and configureVerifier (already defined below), and then verify the program (see verify method in MyVerifier)*/
    } finally {
      verifierInstance.stop() // also doesn't do anything in the current implementation - only needed if you have to "clean-up" in some way
    }

    val exitCode =
      if ( config.error.nonEmpty /* Handling command line options failed */
        || config.exit           /* We had to terminate for some other reason */
        || result != ViperSuccess) /* Verification (including parsing) failed */
        1
      else
        0

    sys.exit(exitCode)
  }

  def createVerifier(fullCmd: String) = {
    verifierInstance = new MyVerifier()  // you will do your work in this class (see below)

    verifierInstance
  }

  def configureVerifier(args: Seq[String]) = {
    verifierInstance.parseCommandLine(args)
    verifierInstance.start() // not strictly needed; the current implementation doesn't do anything

    verifierInstance.config
  }
}

class MyVerifier(timeout: Int = -1) extends BareboneVerifier {
  override def name: String = "MyVerifierName"

  /** Only needed if you want to do something special on (first) invocation of the verifier
    */
  override def start(): Unit = {}

  /** Stops the verifier. Only needed if you have to perform some special clean-up on closing the verifier
    */
  override def stop(): Unit = {}

  /** Verifies a given Viper program and returns the result (success, or a sequence of errors)
    *
    * @param program The program to be verified.
    * @return The verification result.
    */
  override def verify(program: sil.Program): VerificationResult = {
    if (config.printInput.getOrElse(false)) {
      println("Input program:\n" + program)
    }

    if(! util.supportedViperSyntax.isSupportedProgram(program)) {
      return ViperFailure(Seq(Internal(program,InternalReason(program, "Input program uses unsupported Viper features!"))))
    }

    if (config.printTransformed.getOrElse(false)) {
      println("Transformed program:")
      program.domains.foreach(d => println(d))
    }

    val transformer = new MethodTransformer()
    var failures: Seq[VerificationError] = Seq()
    program.methods.foreach(m => {
      // reduction to smt2 formula
      val methodWithInfo = ErrorCreator.addInfoToNodes(m)
      val method = transformer.transform(methodWithInfo)

      if (config.printTransformed.getOrElse(false)) {
        println(method)
      }

      val declarations = DeclarationCollector.collectDeclarations(program, method.locals ++ method.formalArgs ++
                                                                  method.formalReturns)
      //val declarationsString = SmtLibUtils.declarationString(declarations)
      val verificationConditions: Set[VerificationCondition] = WlpStar.wlpStar(method.body, Set())
      val axioms = DeclarationCollector.collectAxioms(program.domains)

      failures = failures ++
        verificationConditions.foldLeft[Seq[VerificationError]](Seq())((acc, validationCondition) => {
          val failure: Option[VerificationError] = hasFailure(validationCondition, declarations, axioms, program)
          if (failure.nonEmpty) {
            acc :+ failure.get
          } else {
            acc
          }
        })
    })

    if (failures.nonEmpty) {
      ViperFailure(failures)
    } else {
      ViperSuccess
    }
  }

  /**
    * checks if a verification condition is violated -> we have an assertion that might fail
    *
    * @param verificationCondition condition that we want to check
    * @param declarations          the declaration of local variables, data-types, functions etc.
    * @param axioms                term that represents the axioms of a domain
    * @param program               the whole program, used in case of an internal error
    * @return
    */
  private def hasFailure(verificationCondition: VerificationCondition,
                          declarations: Seq[Command],
                          axioms: Seq[Term],
                          program: sil.Program): Option[VerificationError] = {
    // solving the smt2 formula with z3
    val query = axioms.map(a => Assert(a)) ++ (Assert(Not(verificationCondition.formula)) :: CheckSat() :: List())
    val z3Response = validateWithZ3(declarations.mkString, query.mkString)
    z3Response match {
      case CheckSatStatus(SatStatus) | CheckSatStatus(UnknownStatus) =>
        Some(verificationCondition.error)
      case CheckSatStatus(UnsatStatus) =>
        None
      case res@_ =>
        Some(Internal(program, InternalReason(program, "Unexpected response from Z3: " + res.toString)))
    }
  }

  /**
    * adds the smt prelude and the given declarations to the query and solves it with z3
    *
    * @param declarations String containing all the variable/function declarations
    * @param query        query that z3 should solve
    * @return
    */
  private def validateWithZ3(declarations: String, query: String) = {
    // FIXME: ugly large method, nearly as ugly as the original verify method...
    // here is a reasonable initial configuration for z3. If you're interested, you can check out the options in the Z3 documentation (some are also visible from z3 /pd etc.)
    var smtPrelude =
      """
        |(set-option :print-success false)
        |(set-info :smt-lib-version 2.0)
        |(set-option :AUTO_CONFIG false)
        |(set-option :pp.bv_literals false)
        |(set-option :MODEL.V2 true)
        |(set-option :smt.PHASE_SELECTION 0)
        |(set-option :smt.RESTART_STRATEGY 0)
        |(set-option :smt.RESTART_FACTOR |1.5|)
        |(set-option :smt.ARITH.RANDOM_INITIAL_VALUE true)
        |(set-option :smt.CASE_SPLIT 3)
        |(set-option :smt.DELAY_UNITS true)
        |(set-option :NNF.SK_HACK true)
        |(set-option :smt.MBQI false)
        |(set-option :smt.QI.EAGER_THRESHOLD 100)
        |(set-option :TYPE_CHECK true)
        |(set-option :smt.BV.REFLECT true)
        |""".stripMargin
    if (timeout > 0) {
      smtPrelude = smtPrelude +
        f"""
           |(set-option :timeout $timeout)
        """.stripMargin
    }
    smtPrelude = smtPrelude +
      """
        |; done setting options
        |
        |""".stripMargin

    // write program to a temporary file (name will be an auto-generated variant of the first parameter string)
    val tmp = File.createTempFile("mytempfile", ".smt2")
    tmp.deleteOnExit()
    val stream = new BufferedOutputStream(new FileOutputStream(tmp))
    val inputString : String = smtPrelude + declarations + query

    if(config.printSMT.getOrElse(false)) { // print the smt output if the command-line option was specified
      println(inputString)
    }
    stream.write(inputString.getBytes)
    stream.close()

    val z3Path : String = config.z3executable.toOption.get // this option is always set (possibly to the default of "z3"), so "get" is safe

    // gather any Z3 arguments provided on the command-line
    val userProvidedZ3Args: Array[String] = config.z3Args.toOption match {
      case None =>
        Array()
      case Some(args) =>
        args.split(' ').map(_.trim)
    }

    // store the outputs which come from stdout, stderr
    var result: String = ""
    var resulterr: String = ""
    def out(input: java.io.InputStream) {
      result += convertStreamToString(input)
      input.close()
    }
    def err(in: java.io.InputStream) {
      resulterr += convertStreamToString(in)
      in.close()
    }

    val defaultOptions = Seq("-smt2") // you may want to pass more options to z3 here, or do it via the command-line argument z3Args

    // run Z3, passing the tmp file as input:
    (Seq(z3Path) ++ defaultOptions ++ userProvidedZ3Args ++ Seq(tmp.getAbsolutePath)).run(new ProcessIO(_.close(), out, err)).exitValue() // .exitValue() causes us to block until the process terminates

    // you might also want to check "resulterr" (stderr) for potential errors; here we only parse stdout using the scala-smtlib library
    // as an alternative, you could read the Z3 response directly and parse it by hand (you should get "unsat", "sat" or "unknown", if everything worked)
    val lexer = new smtlib.lexer.Lexer(new java.io.StringReader(result))
    val parser = new smtlib.parser.Parser(lexer)

    // we use a scala-smtlib function to parse the response
    parser.parseCheckSatResponse
  }

  // utility method for reading the input stream into a String
  def convertStreamToString(is: java.io.InputStream) : String = {
    val s = new java.util.Scanner(is).useDelimiter("\\A")
    if (s.hasNext) s.next() else ""
  }

}
