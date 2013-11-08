package qa.state

import qa.matrix._
import qa.util._

private final class CompositeState(_states: List[LeafState]) extends State {

  val states = _states.sorted(LeafStateOrdering)

  require(2 <= states.size, "At least 2 Leaf States required")

  {
    require(!states.exists(
      _.qubits.length != numberOfQubits))
  }

  override def numberOfQubits: Int =
    states.head.qubits.length

  override def *(coefficient: Double): CompositeState = {
    val newStates =
      for (state <- states)
        yield (coefficient * state).asInstanceOf[LeafState]

    new CompositeState(newStates)
  }

  override def probability: Double =
    states map { _.probability } sum

  override private[state] def statesList: List[State] =
    states.toList.asInstanceOf[List[State]]

  override def toString: String =
    states.mkString(" ")

  override def equals(other: Any): Boolean = {
    other match {
      case other: CompositeState => {
        (states.size == other.states.size) && (states.forall(other.states.contains(_)))
      }
      case _ => false
    }
  }

  override def ==(other: State): Boolean =
    equals(other)

  override def toVector: Vector = {
    var v = statesList.head.toVector

    for (s <- statesList.tail)
      v = v + s.toVector

    v
  }

  override def measure: State = {
    val result = State.rng.nextDouble * probability

    measuredState(result)
  }

  def measureSingleQubit(index: Int): (Int, State) = {
    var probabilitySum = 0D
    val result = State.rng.nextDouble * probability

    states foreach { ls => if (0 == ls.qubits(index)) probabilitySum += ls.probability }

    val measured = if (probabilitySum >= result) 0 else 1

    val remaining: List[LeafState] =
      for {
        ls <- states
        if (measured == ls.qubits(index))
      } yield ls

    val newState = State.consolidate(remaining).renormalize(probability)

    (measured, newState)
  }

  override def inspectQubit(index: Int) = {
    require(index >= 0)
    require(index < numberOfQubits)

    var oneQubitState = 0D * State(0) + 0D * State(1)
    states foreach { s =>
      oneQubitState += s.coefficient * State(s.qubits(index))
    }

    oneQubitState.renormalize(probability)
  }

  private def measuredState(result: Double, remaining: List[LeafState] = states.toList, sum: Double = 0): State = {
    remaining match {
      case head :: Nil => State(head.qubits)
      case head :: tail => {
        val newSum = sum + head.probability
        if (newSum >= result)
          State(head.qubits)
        else
          measuredState(result, tail, newSum)
      }
      case _ => sys.error("unreachable")
    }
  }

}
