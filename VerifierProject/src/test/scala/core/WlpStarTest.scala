package core

import org.scalatest.FunSuite

/**
  * Created by jan on 07.04.17.
  */
class WlpStarTest extends FunSuite {

  val dir = "src/test/resources/WlpStar/"
  val fileRegex = ".*-amfc-(\\d+).vpr".r

  for (f <- TestUtils.getListOfVprFiles(dir)) {
    test("WlpStarTest-" + f) {
      val args: Array[String] = Array(dir + f)
      TestingFrontend.execute(args)
      val res = TestingFrontend.getResult

      val mightFailCount: Int = (for (m <- fileRegex.findFirstMatchIn(f)) yield m.group(1)).get.toInt

      TestUtils.assertAssertionMightFail(res, mightFailCount)
    }
  }

}
