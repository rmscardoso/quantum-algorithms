package qa.matrix

import UnitVector._
import qa.util._

class UnitVector(val dimensions: Int, val index: Int) extends Vector(buildUnitVectorList(dimensions, index)) {

  require(index > -1)
  require(dimensions > index)
  
  def this(v: Vector) {
    this(v.toList.size, v.toList.indexOf(1D))
    require(1D ~= v.toList.sum)
  }
  
}

object UnitVector {
  
  def apply(dimensions: Int, index: Int): UnitVector =
    new UnitVector(dimensions, index)

  private def buildUnitVectorList(dimensions: Int, number: Int): List[List[Double]] =
    (for (i <- 0 until dimensions)
      yield List(if (i == number) 1D else 0D)).toList
      
}
