package core

import org.scalatest.FunSuite

/**
  * Created by jan on 21.04.17.
  */
class GeneralTest extends FunSuite {

  val dir = "src/test/resources/"

  for (f <- TestUtils.getListOfVprFiles(dir)) {
    test("GeneralTest-" + f) {
      runTest(f)
    }
  }

  // for debugging purposes...
  ignore("dummy") {
    // paste the file-name here
    val f = "lecture-peano.vpr"

    runTest(f)
  }

  private def runTest(fileName: String) = {
    val args: Array[String] = Array(dir + fileName)
    TestingFrontend.execute(args)
    val res = TestingFrontend.getResult
    println(res)
  }
}

