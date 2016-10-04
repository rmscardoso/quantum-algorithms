package qa.matrix

import org.scalatest._

class UnitVectorTest extends FlatSpec with Matchers {

  "A UnitVector" should "equal the corresponding Vector" in {
    UnitVector(4, 0) should equal(Vector(1, 0, 0, 0))
    UnitVector(4, 1) should equal(Vector(0, 1, 0, 0))
    UnitVector(4, 2) should equal(Vector(0, 0, 1, 0))
    UnitVector(4, 3) should equal(Vector(0, 0, 0, 1))
  }
  
  it should "give its number" in {
    UnitVector(4, 1).index should equal(1)
    UnitVector(4, 3).index should equal(3)
  }
  
  it can "be constructed from a Vector" in {
    val uv = new UnitVector(Vector(0, 1))
    uv.index should equal(1)
    
    new UnitVector(Vector(1, 0)).index should equal(0)
  }

}