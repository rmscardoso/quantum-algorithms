package qa.util

import org.scalatest._

class UtilTest extends FlatSpec with Matchers {

  "A double" should "nearly equal another double" in {
    val d1 = 0.3
    val d2 = d1 + DoublePrecision
    val d3 = d2 + DoublePrecision

    (d1 ~= d2) should be(true)
    (d2 ~= d3) should be(true)
    (d1 ~= d3) should be(false) // violates transitivity concept in equals, but that's okay 
  }

}
