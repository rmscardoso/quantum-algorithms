package qa.operator

import qa.util._
import qa.matrix._
import qa.state._

abstract class Operator(val matrix: Matrix) {

  require(2 == matrix.rows && 2 == matrix.columns, "Operators are 2x2 Matrices")

  def apply(s: State): State =
    apply(0 until s.numberOfQubits)(s)

  def apply(qubits: Int*)(s: State): State =
    apply(qubits.toList)(s)

  def apply(qubits: Range)(s: State): State =
    apply(qubits.toList)(s)

  def apply(qubits: List[Int])(s: State): State =
    State.fromVector(expandAt(qubits, s.numberOfQubits) * s.toVector)

  private[operator] def expandAll(i: Int): Matrix =
    expandAt((0 until i).toList, i)

  private def expandAt(qubits: List[Int], numberOfQubits: Int): Matrix = {
    require(numberOfQubits >= 1)

    var m = Matrix(1)
    for(i <- 0 until numberOfQubits) {
      m = m.tensor(if(qubits.contains(i)) matrix else I.matrix)
    }
    m
  }

}
