package core

import org.scalatest.FunSuite

/**
  * Created by jan on 07.04.17.
  */
class WlpStarTest extends FunSuite {

  val dir = "src/test/resources/WlpStar/"

  for (f <- TestUtils.getListOfVprFiles(dir)) {
    test("WlpStarTest-" + f) {
      val args: Array[String] = Array(dir + f)
      Main.main(args)
    }
  }

}
