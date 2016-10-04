package qa.state

import qa.util._
import qa.matrix._

private[qa] final class LeafState(val coefficient: Double, val qubits: List[Int]) extends State {

  require(0 < numberOfQubits, "needs at least one qubit")

  qubits.foreach(qubit =>
    require(0 == qubit || 1 == qubit))

  override def numberOfQubits: Int =
    qubits.length

  override def *(otherCoefficient: Double): LeafState = {
    new LeafState(coefficient * otherCoefficient, qubits)
  }

  override def probability =
    coefficient * coefficient

  override private[state] def statesList: List[State] =
    List[State] { this }

  override def equals(other: Any): Boolean =
    other match {
      case other: LeafState =>
        (coefficient ~= other.coefficient) && (qubits == other.qubits)
      case _ => false
    }

  override def ==(other: State): Boolean =
    equals(other)

  override def toString: String = {
    val coStr =
      if (0 > coefficient)
        "- " + math.abs(coefficient).toString
      else
        "+ " + coefficient.toString

    qubits.mkString(coStr + "|", ", ", ">")
  }

  override def toVector: Vector = {
    var v: Vector = if (0 == qubits.head) Vector(1, 0) else Vector(0, 1)

    for (q <- qubits.tail) {
      v = v tensor (if (0 == q) Vector(1, 0) else Vector(0, 1))
    }

    coefficient * v
  }

  override def measure: LeafState =
    this

  override def measureSingleQubit(index: Int): (Int, LeafState) = {
    require(index < qubits.size)

    (qubits(index), this)
  }
  
  override def inspectQubit(index: Int) = {
    require(numberOfQubits > index)
    if(0 == qubits(index)) 
      State(0)
    else 
      State(1)
  }

  private[state] def calcBitValue(rest: List[Int] = qubits): Int = {
    rest match {
      case head :: Nil => head
      case head :: tail => head * math.pow(2, tail.size).toInt + calcBitValue(tail)
      case _ => sys.error("unreachable")
    }
  }

}

private object LeafStateOrdering extends Ordering[LeafState] {
  override def compare(x: LeafState, y: LeafState): Int = {
    val xVal = x.calcBitValue()
    val yVal = y.calcBitValue()
    if (xVal < yVal)
      -1
    else if (xVal == yVal)
      0
    else 1
  }
}