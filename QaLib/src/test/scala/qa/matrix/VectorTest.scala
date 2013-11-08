package qa.matrix

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class VectorTest extends FlatSpec with ShouldMatchers {

  "A Vector's sum" should "be another Vector" in {
    val (a, b, c, d) = (1, 2, 3, 4)
    val v1 = Vector(a, b)
    val v2 = Vector(c, d)

    val expected = Vector(a + c, b + d)

    (v1 + v2) should equal(expected)
    (v1 + v2).isInstanceOf[Vector] should be(true)
  }

  "A Vector's product with a Double" should "be another Vector" in {
    val (a, b) = (1, 2)
    val v = Vector(a, b)

    val co = 2.5
    (co * v) should equal(Vector(co * a, co * b))
    (co * v).isInstanceOf[Vector] should be(true)
  }

  "A Vector's tensor product with another Vector" should "be yet another Vector" in {
    val (a, b, c, d) = (1, 2, 3, 4)
    val v1 = Vector(a, b)
    val v2 = Vector(c, d)

    val expected = Vector(a * c, a * d, b * c, b * d)

    (v1 tensor v2) should equal(expected)
    (v1 tensor v2).isInstanceOf[Vector] should be(true)
  }
  
  "A Vector" should "give a List of its entries" in {
    Vector(0, 1).toList should equal(List(0D, 1D))
    Vector(0.3, 0.3, 0.3, 0.3).toList should equal(List(0.3, 0.3, 0.3, 0.3))
  }

}