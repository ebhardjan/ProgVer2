package core

import org.scalatest.FunSuite

/**
  * Created by jan on 07.04.17.
  */
class WlpStarTest extends FunSuite {

  val dir = "src/test/resources/WlpStar/"

  for (f <- TestUtils.getListOfVprFiles(dir)) {
    test("WlpStarTest-" + f) {
      TestUtils.runTest(dir, f)
    }
  }

  // for debugging purposes...
  ignore("dummy") {
    // paste the file-name here
    val f = "simple-amfc-1.vpr"

    TestUtils.runTest(dir, f)
  }

}

