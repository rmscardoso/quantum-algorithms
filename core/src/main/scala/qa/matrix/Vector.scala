package qa.matrix

//TODO: resolve Naming conflict with collection.Vector!
class Vector(entries: List[List[Double]]) extends Matrix(entries) {

  def +(other: Vector): Vector =
    super.+(other).toVector

  override def *(d: Double): Vector =
    super.*(d).toVector

  def tensor(v: Vector): Vector =
    super.tensor(v).toVector

  override def toString: String =
    rep.transpose.flatten.mkString("Vector(", ",", ")")
    
  def toList = rep.transpose.flatten

}

object Vector {

  def apply(entries: Double*): Vector =
    new Vector(List(entries.toList).transpose)

}