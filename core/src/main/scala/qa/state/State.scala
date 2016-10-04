package qa.state

import qa.util._
import State._
import math._
import collection.mutable.{ HashSet => MutableSet }
import qa.matrix._
import util.Random

abstract case class State() {

  def probability: Double

  def numberOfQubits: Int

  private[state] def statesList: List[State]

  def *(coefficient: Double): State

  def +(other: State): State =
    consolidate(this.statesList ::: other.statesList)

  def -(other: State): State =
    this + (-1 * other)

  final def isNormalized: Boolean =
    1 == probability

  def toVector: Vector

  def measure: State

  def measureSingleQubit(index: Int): (Int, State)

  def tensor(other: State): State =
    fromVector(toVector tensor other.toVector)

  def ⊗(other: State): State = tensor(other)

  def ==(other: State): Boolean

  def inspectQubit(index: Int): State

  def renormalize(to: Double): State = 
    math.sqrt(to / probability) * this

}

object State {

  var rng: Random = new Random

  def apply(qubits: List[Int]): State =
    new LeafState(1, qubits)

  def apply(qubits: Int*): State =
    new LeafState(1, qubits.toList)

  private[state] def consolidate(states: List[State]): State = {
    val consolidated = new MutableSet[LeafState] with ConsolidatesStates
    for (state <- states) {
      state match {
        case state: LeafState =>
          consolidated += state

        case state: CompositeState => {
          for (subState <- state.states)
            consolidated += subState
        }
      }
    }

    val leafStates = consolidated.toList

    leafStates.size match {
      case 0 => sys.error("empty state")
      case 1 => leafStates.head
      case _ => new CompositeState(leafStates)
    }
  }

  private trait ConsolidatesStates extends MutableSet[LeafState] {
    abstract override def +=(state: LeafState): this.type = {
      val foundOpt = find(_.qubits == state.qubits)

      if (foundOpt.isEmpty)
        super.+=(state)
      else {
        val found = foundOpt.get
        super.-=(found)
        super.+=(new LeafState(state.coefficient + found.coefficient, state.qubits))
      }

      this
    }

  }

  def fromVector(v: qa.matrix.Vector): State = {
    var states = List[State]()

    for (i <- 0 until v.rows) {
      if (!(0D ~= v.rep(i)(0)))
        states = v.rep(i)(0) * fromUnitVector(UnitVector(v.rows, i)) :: states
    }

    consolidate(states)
  }

  private def fromUnitVector(u: UnitVector): LeafState = {
    val noOfQubits = lg2(u.dimensions)

    var qubits = List[Int]()
    var remaining = u.index

    for (i <- 0 until noOfQubits) {
      qubits = remaining % 2 :: qubits
      remaining /= 2
    }

    new LeafState(1, qubits)
  }

  private def lg2(d: Int): Int = //dangerous!
    (log(d) / log(2)).intValue()

}