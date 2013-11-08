package qa

package object matrix {
  class Multiplier(d: Double) {
    def this(i: Int) =
      this(i.toDouble)

    def *(m: Matrix): Matrix =
      m * d

    def *(v: Vector): Vector =
      v * d
  }

  implicit def doubleToMultiplier(d: Double): Multiplier =
    new Multiplier(d)

  implicit def intToMultiplier(i: Int): Multiplier =
    new Multiplier(i)

  implicit def intToDoubleList(i: Int): List[Double] =
    List(i.doubleValue())

  implicit def doubleToDoubleList(d: Double): List[Double] =
    List(d)

  implicit def intListToDoubleList(intList: List[Int]): List[Double] =
    intList.map(_.toDouble)

  implicit def productToDoubleList(p: Product): List[Double] = {
    (p.productIterator map {
      case i: Int => i.doubleValue()
      case d: Double => d
    }) toList
  }

  implicit def productToMatrixList(p: Product): List[Matrix] = {
    (p.productIterator map {
      case m: Matrix => m
      case a: Any => {
        println(a.getClass())
        throw new MatchError
      }
    }) toList
  }
}