package core

import org.scalatest.FunSuite

/**
  * Created by jan on 30.04.17.
  */
class RealApplicationTests extends FunSuite {

  val dir = "src/test/resources/RealApplicationTests/"

  for (f <- TestUtils.getFilteredListOfVprFiles(dir)) {
    test("RealApplicationTests-" + f) {
      TestUtils.runTest(dir, f)
    }
  }

}
