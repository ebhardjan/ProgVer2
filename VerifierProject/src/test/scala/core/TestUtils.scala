package core

import java.io.File

import viper.silver.frontend.SilFrontend
import viper.silver.verifier.{AbstractVerificationError, Failure, Success, VerificationResult}

import scala.util.matching.Regex

/**
  * Created by jan on 07.04.17.
  *
  * Testing utility methods
  */
object TestUtils {

  val fileRegex: Regex = ".*-amfc-(\\d+).vpr".r

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

  /**
    * Get a list of all the vpr files that match the might fail filname format in a given directory
    *
    * @param directoryPath directory where to look for vpr files
    * @return list of paths of the vpr files
    */
  def getFilteredListOfVprFiles(directoryPath: String): List[String] = {
    val directory = new File(directoryPath)
    if (directory.exists && directory.isDirectory) {
      directory.listFiles
        .filter(f => f.isFile)
        .filter(f => f.getName.matches(fileRegex.regex))
        .map(f => f.getName)
        .toList
    } else {
      List()
    }
  }

  def assertAssertionMightFail(verificationResult: VerificationResult, failedAssertionCount: Int): Unit = {
    verificationResult match {
      case Failure(errors) =>
        val actualCount = errors.count(e => e.isInstanceOf[AbstractVerificationError])
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

  /**
    * Runs a particular test and asserts that the correct number of "might-fails" are observed
    *
    * @param dir      the directory where the viper test file is located
    * @param fileName the name of the viper test file
    */
  def runTest(dir: String, fileName: String): Unit = {
    val args: Array[String] = Array(dir + fileName)
    TestingFrontend.execute(args)
    val res = TestingFrontend.getResult

    val mightFailCount: Int = (for (m <- fileRegex.findFirstMatchIn(fileName)) yield m.group(1)).get.toInt

    TestUtils.assertAssertionMightFail(res, mightFailCount)
  }

}

/**
  * similar to the Main object in MyVerifier.scala
  */
object TestingFrontend extends SilFrontend {

  protected var verifierInstance: MyVerifier = _

  def createVerifier(fullCmd: String): MyVerifier = {
    // we set the timeout to 20s -> the z3 smt solver will stop after about 5 seconds max
    // so there won't be tests that run infinitely long
    verifierInstance = new MyVerifier(timeout = 20000)

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
