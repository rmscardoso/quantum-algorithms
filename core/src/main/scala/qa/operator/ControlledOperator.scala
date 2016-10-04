package qa.operator

import qa.state._
import qa.matrix._
import ControlledOperator._

abstract class ControlledOperator(val op: Operator) {

  def apply(controllQubits: Int*)(qubit: Int)(s: State): State = {
    require(qubit < s.numberOfQubits)
    require(!controllQubits.contains(qubit))

    require(0 < controllQubits.size && controllQubits.size == controllQubits.toSet.size)
    for (i <- controllQubits)
      require(i < s.numberOfQubits)

    val qubitsMatrix = matrix(controllQubits.toList, qubit)
    val involvedQubits = qubit :: controllQubits.toList

    var m = Matrix(1)
    for (i <- 0 until involvedQubits.min) {
      m = m tensor I.matrix
    }
    m = m tensor qubitsMatrix
    for (i <- (involvedQubits.max + 1) until s.numberOfQubits) {
      m = m tensor I.matrix
    }

    State.fromVector(m * s.toVector)
  }

  def matrix(controllingQubits: List[Int], controlledQubit: Int): Matrix = {
    val involvedQubits = controlledQubit :: controllingQubits
    val min = involvedQubits.min

    involvedQubitsMatrix(controllingQubits map { _ - min }, controlledQubit - min)
  }

  def involvedQubitsMatrix(controllingQubits: List[Int], controlledQubit: Int): Matrix = {
    val numberOfQubits = (controlledQubit :: controllingQubits).max + 1

    val matrix = for (s <- allQubitStates(numberOfQubits)) yield {
      val state = State(s)

      if (shouldApplyOperator(s, controllingQubits)) {
        op(controlledQubit)(state).toVector.toList
      } else {
        state.toVector.toList
      }
    }

    new Matrix(matrix)
  }

}

object ControlledOperator {

  def shouldApplyOperator(statesQubits: List[Int], controllingQubits: List[Int]): Boolean = {
    controllingQubits forall { 1D == statesQubits(_) }
  }

  def allQubitStates(count: Int) = {
    (for (i <- 0 until math.pow(2, count).intValue) yield bitList(i, count)).toList
  }

  def bitList(value: Int, dim: Int): List[Int] = {
    var l = List.empty[Int]
    var remaining = value
    for (i <- 0 until dim) {
      l = (if (0 == remaining % 2) 0 else 1) :: l
      remaining /= 2
    }
    l
  }

}
