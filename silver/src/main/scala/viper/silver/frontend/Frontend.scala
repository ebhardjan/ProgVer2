/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.frontend

import java.nio.file.{Files, Path}
import scala.io.Source
import viper.silver.ast.Program
import viper.silver.verifier.{AbstractError, Failure, VerificationResult, Verifier}


/** Represents one phase of a frontend */
case class Phase(name: String, action: () => Unit)

/** A translator for some programming language that produces a SIL program (which then in turn can be verified using a
  * SIL verifier).

  */
trait Frontend {

  /** Initialize this translator with a given verifier. Only meant to be called once. */
  def init(verifier: Verifier)

  /**
   * Reset the translator, and set the input program. Can be called many times to verify multiple programs
   * using the same verifier.
   */
  def reset(input: Seq[Path])

  /**
   * Reset any messages recorded internally (errors from previous program translations, etc.)
   */
  def resetMessages ()


  /**
   * Run the verification on the input and return the result.  This is equivalent to calling all the phases and then
   * returning result.
   */
  def run(): VerificationResult = {
    phases.foreach(p => p.action())
    result
  }

  /** The phases of this frontend which have to be executed in the order given by the list. */
  val phases: Seq[Phase]

  /**
   * The result of the verification attempt (only available after parse, typecheck, translate and
   * verify have been called).
   */
  def result: VerificationResult

  val logger = org.apache.log4j.Logger.getLogger(this.getClass.getName)
  logger.setLevel(org.apache.log4j.Level.INFO)

  val loggerForIde = org.apache.log4j.Logger.getLogger(this.getClass.getName+"_IDE")
  loggerForIde.setLevel(org.apache.log4j.Level.INFO)
}

trait SinglePhase extends Frontend {
  val phases = List(
    Phase("singlePhase", runPhase)
  )
  def runPhase()
}

trait DefaultPhases extends Frontend {

  val phases = List(
    Phase("parse", parse),
    Phase("typecheck", typecheck),
    Phase("translate", translate),
    Phase("verify", verify))

  /** Parse the program. */
  def parse()

  /** Type-check the program. */
  def typecheck()

  /** Translate the program to SIL. */
  def translate()

  /** Verify the SIL program using the verifier. */
  def verify()

}

trait SingleFileFrontend {
  def reset(file: Path)

  def reset(files: Seq[Path]) {
    files match {
      case f :: Nil => reset(f)
      case _ => sys.error("This frontend can only handle single files.")
    }
  }
}

/** A default implementation of a translator that keeps track of the state of the translator.
  */
trait DefaultFrontend extends Frontend with DefaultPhases with SingleFileFrontend {
  sealed trait Result[+A]
  case class Succ[+A](a: A) extends Result[A]
  case class Fail(errors: Seq[AbstractError]) extends Result[Nothing]

  protected type ParserResult <: AnyRef
  protected type TypecheckerResult <: AnyRef

  protected var _state: TranslatorState.Value = TranslatorState.Initial
  protected var _verifier: Option[Verifier] = None
  protected var _input: Option[String] = None
  protected var _inputFile: Option[Path] = None
  protected var _errors: Seq[AbstractError] = Seq()
  protected var _verificationResult: Option[VerificationResult] = None
  protected var _parseResult: Option[ParserResult] = None
  protected var _typecheckResult: Option[TypecheckerResult] = None
  protected var _program: Option[Program] = None

  def parserResult: ParserResult = _parseResult.get
  def typecheckerResult: TypecheckerResult = _typecheckResult.get
  def translatorResult: Program = _program.get

  def state = _state

  override def init(verifier: Verifier) {
    _state = TranslatorState.Initialized
    _verifier = Some(verifier)
  }

  override def reset(input: Path) {
    if (state < TranslatorState.Initialized) sys.error("The translator has not been initialized.")
    _state = TranslatorState.InputSet
    _inputFile = Some(input)
    _input = Some(Source.fromInputStream(Files.newInputStream(input)).mkString)
    _errors = Seq()
    _program = None
    _verificationResult = None
    _parseResult = None
    _typecheckResult = None
    resetMessages()
  }

  protected def mapVerificationResult(in: VerificationResult): VerificationResult

  protected def doParse(input: String): Result[ParserResult]

  protected def doTypecheck(input: ParserResult): Result[TypecheckerResult]

  protected def doTranslate(input: TypecheckerResult): Result[Program]

  protected def printOutline(program: Program)

  override def parse() {
    if (state < TranslatorState.InputSet) sys.error("The translator has not been initialized, or there is no input set.")
    if (state >= TranslatorState.Parsed) return
    doParse(_input.get) match {
      case Succ(r) => _parseResult = Some(r)
      case Fail(e) => _errors ++= e
    }
    _state = TranslatorState.Parsed
  }

  override def typecheck() { // typecheck and translate (if successful)
    if (state >= TranslatorState.Typechecked || _errors.nonEmpty) return
    parse()
    if (_errors.nonEmpty) {
      _state = TranslatorState.Typechecked
      return
    }
    doTypecheck(_parseResult.get) match {
      case Succ(r) => _typecheckResult = Some(r)
      case Fail(e) => _errors ++= e
    }
    _state = TranslatorState.Typechecked
  }

  override def translate() {
    if (state >= TranslatorState.Translated || _errors.nonEmpty) return
    typecheck()
    if (_errors.nonEmpty) {
      _state = TranslatorState.Translated
      return
    }
    doTranslate(_typecheckResult.get) match {
      case Succ(r) => _program = Some(r)
      case Fail(e) => _errors ++= e
    }
    _state = TranslatorState.Translated
  }

  override def verify() {
    if (state >= TranslatorState.Verified || _errors.nonEmpty) return
    translate()
    if (_errors.nonEmpty) {
      _state = TranslatorState.Verified
      return
    }
//    _verifier.get.start()

    _verificationResult = Some(mapVerificationResult(_verifier.get.verify(_program.get)))
    assert(_verificationResult != null)

//    _verifier.get.stop()

    printOutline(_program.get)

    _state = TranslatorState.Verified
  }

  override def result = {
    if (_errors.isEmpty) {
      require(state >= TranslatorState.Verified)
      _verificationResult.get
    }
    else {
      Failure(_errors)
    }
  }
}

object TranslatorState extends Enumeration {
  type TranslatorState = Value
  val Initial, Initialized, InputSet, Parsed, Typechecked, Translated, Verified = Value
}
