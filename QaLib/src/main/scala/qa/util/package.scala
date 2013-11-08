package qa

import math._

package object util {

  val OS2 = 1 / sqrt(2)

  val DoublePrecision = 1D / pow(10, 6)

  class DoubleWithAlmostEqual(d: Double) {
    def ~=(other: Double): Boolean =
      abs(d - other) <= DoublePrecision
  }

  implicit def ~=(d: Double): DoubleWithAlmostEqual =
    new DoubleWithAlmostEqual(d)

}