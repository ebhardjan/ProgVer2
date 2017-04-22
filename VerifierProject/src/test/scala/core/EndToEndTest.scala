package core

import org.scalatest.FunSuite

/**
  * Created by jan on 21.04.17.
  */
class EndToEndTest extends FunSuite {

  val dir = "src/test/resources/EndToEnd/"

  for (f <- TestUtils.getListOfVprFiles(dir)) {
    test("EndToEndTest-" + f) {
      TestUtils.runTest(dir, f)
    }
  }

  // for debugging purposes...
  ignore("dummy") {
    // paste the file-name here
    val f = "whileInvariantPreservedNegative-amfc-1.vpr"

    TestUtils.runTest(dir, f)
  }

}

