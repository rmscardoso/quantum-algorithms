package qa

sealed trait State {
  def states: Seq[State]
  def +(other: State): State
}

case class BaseState(coefficient: Double, qubits: Seq[Int]) extends State with Ordered[BaseState] {
  override def states: Seq[State] =
    Seq(this)

  override def +(other: State): State =
    other match {
      case other: BaseState =>
        if(qubits == other.qubits)
          BaseState(coefficient + other.coefficient, qubits)
        else
          SuperposedState(Seq(this, other))
      case other: SuperposedState =>
        other + this
    }

  override def compare(that: BaseState): Int = {
    require(qubits.size == that.qubits.size, "States must be of equal number of qubits")

    for(i <- qubits.indices)
      if(qubits(i) < that.qubits(i))
        return -1
      else if(qubits(i) > that.qubits(i))
        return 1

    0
  }
}

class SuperposedState private (val states: Seq[BaseState]) extends State {
  require(states == states.sorted, "states must be sorted")
  require(states.map(_.qubits).groupBy(identity).forall(_._2.length == 1), "base states must be unique")

  override def +(other: State): State =
    other match {
      case other: BaseState =>
        SuperposedState(states :+ other)
      case other: SuperposedState =>
        SuperposedState(states ++ other.states)
    }


  def canEqual(other: Any): Boolean = other.isInstanceOf[SuperposedState]

  override def equals(other: Any): Boolean = other match {
    case that: SuperposedState =>
      (that canEqual this) &&
        states == that.states
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(states)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = s"SuperposedState($states)"
}
object SuperposedState {
  def apply(states: Seq[BaseState]): SuperposedState = {
    val consolidated = states
      .groupBy(_.qubits)
      .mapValues{ (groupedStates: Seq[BaseState]) =>
        if(groupedStates.tail.isEmpty)
          groupedStates.head
        else
          groupedStates
            .reduce{ (l: State, r: State) => l + r }
            .asInstanceOf[BaseState] }
      .values
      .toSeq
      .sorted

    new SuperposedState(consolidated)
  }
}

object State {
  val sqrt2 = math.sqrt(2)

  def apply(qubits: Int*): State = BaseState(1, qubits)

  implicit class StateCoefficient(coeff: Double) {
    def *(s: State): State = s match {
      case ls: BaseState => ls.copy(coefficient = ls.coefficient * coeff)
    }
  }
}
