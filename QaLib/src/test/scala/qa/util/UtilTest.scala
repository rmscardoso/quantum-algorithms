package qa.util

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import util._

@RunWith(classOf[JUnitRunner])
class UtilTest extends FlatSpec with ShouldMatchers {

  "A double" should "nearly equal another double" in {
    val d1 = 0.3
    val d2 = d1 + DoublePrecision
    val d3 = d2 + DoublePrecision

    (d1 ~= d2) should be(true)
    (d2 ~= d3) should be(true)
    (d1 ~= d3) should be(false) // violates transitivity concept in equals, but that's okay 
  }

}
