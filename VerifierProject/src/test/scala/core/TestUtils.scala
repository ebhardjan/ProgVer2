package core

import java.io.File

import viper.silver.frontend.SilFrontend
import viper.silver.verifier.errors.AssertFailed
import viper.silver.verifier.{Failure, Success, VerificationResult}

/**
  * Created by jan on 07.04.17.
  *
  * Testing utility methods
  */
object TestUtils {

  /**
    * Get a list of all the vpr files in a given directory
    *
    * @param directoryPath directory where to look for vpr files
    * @return list of paths of the vpr files
    */
  def getListOfVprFiles(directoryPath: String): List[String] = {
    val directory = new File(directoryPath)
    if (directory.exists && directory.isDirectory) {
      directory.listFiles
        .filter(f => f.isFile)
        .filter(f => f.getName.contains(".vpr"))
        .map(f => f.getName)
        .toList
    } else {
      List()
    }
  }

  def assertAssertionMightFail(verificationResult: VerificationResult, failedAssertionCount: Int): Unit = {
    verificationResult match {
      case Failure(errors) =>
        val actualCount = errors.count(e => e.isInstanceOf[AssertFailed])
        assert(actualCount == failedAssertionCount,
          s"might-fail assertion count should be $failedAssertionCount but was $actualCount")
      case Success =>
        if (failedAssertionCount == 0) {
          assert(true)
        } else {
          assert(assertion = false, s"no might-fail assertion found, there should be $failedAssertionCount")
        }
      case _ => throw new RuntimeException("Unexpected verification result")
    }
  }

}

/**
  * similar to the Main object in MyVerifier.scala
  */
object TestingFrontend extends SilFrontend {

  protected var verifierInstance: MyVerifier = _

  def createVerifier(fullCmd: String): MyVerifier = {
    verifierInstance = new MyVerifier()

    verifierInstance
  }

  def configureVerifier(args: Seq[String]): Config = {
    verifierInstance.parseCommandLine(args)
    verifierInstance.start()

    verifierInstance.config
  }

  def getResult: VerificationResult = {
    _verificationResult.getOrElse(throw new RuntimeException("No verification result available"))
  }

}
