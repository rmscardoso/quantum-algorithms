package qa.state

import breeze.linalg.{DenseVector, kron}

import scala.math._

sealed trait State {
  def states: Seq[State]
  def +(other: State): State
  def *(d: Double): State
  def toString: String
  private[qa] def toVector: DenseVector[Double]

  def -(other: State): State =
    this + (-1 * other)
}

object State {
  val sqrt2 = math.sqrt(2)

  private[state] case class IndividualState(coefficient: Double, qubits: Seq[Int]) extends State with Ordered[IndividualState] {
    require(qubits.forall{ q => q == 0 || q == 1 })

    override def states: Seq[State] =
      Seq(this)

    override def +(other: State): State =
      other match {
        case other: IndividualState =>
          if(qubits == other.qubits)
            IndividualState(coefficient + other.coefficient, qubits)
          else
            SuperposedState(Seq(this, other))
        case other: SuperposedState =>
          other + this
      }

    override def *(d: Double): IndividualState =
      copy(coefficient = coefficient * d)

    override private[qa] def toVector: DenseVector[Double] = {
      var v = if (0 == qubits.head) DenseVector[Double](1, 0) else DenseVector[Double](0, 1)

      for (q <- qubits.tail) {
        val next = if (0 == q) DenseVector[Double](1, 0) else DenseVector[Double](0, 1)
        v = kron(v.asDenseMatrix, next.asDenseMatrix).toDenseVector
      }

      v * coefficient
    }

    override def compare(that: IndividualState): Int = {
      require(qubits.size == that.qubits.size, "States must be of equal number of qubits")

      for(i <- qubits.indices)
        if(qubits(i) < that.qubits(i))
          return -1
        else if(qubits(i) > that.qubits(i))
          return 1

      0
    }

    override def toString = s"$coefficient|${qubits.mkString(",")}>"
  }

  private[state] class SuperposedState(val states: Seq[IndividualState]) extends State {
    require(states == states.sorted, "states must be sorted")
    require(states.map(_.qubits).groupBy(identity).forall(_._2.length == 1), "base states must be unique")

    override def +(other: State): State =
      other match {
        case other: IndividualState =>
          SuperposedState(states :+ other)
        case other: SuperposedState =>
          SuperposedState(states ++ other.states)
      }

    override def *(d: Double): SuperposedState =
      new SuperposedState(states.map(_ * d))

    override private[qa] def toVector: DenseVector[Double] =
      states.map(_.toVector).reduce(_ + _)

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

    override def toString =
      states.head.toString + states.tail.map { s =>
        if(s.coefficient < 0)
          " - " + s * -1
        else
          " + " + s
      }.mkString("")
  }
  private[state] object SuperposedState {
    def apply(states: Seq[IndividualState]): SuperposedState = {
      val consolidated = states
        .groupBy(_.qubits)
        .mapValues{ (groupedStates: Seq[IndividualState]) =>
          if(groupedStates.tail.isEmpty)
            groupedStates.head
          else
            groupedStates
              .reduce{ (l: State, r: State) => l + r }
              .asInstanceOf[IndividualState] }
        .values
        .toSeq
        .sorted

      new SuperposedState(consolidated)
    }
  }

  def apply(qubits: Int*): State =
    IndividualState(1, qubits)

  def fromVector(vector: DenseVector[Double]): State = {
    def unitVector(index: Int, dimensions: Int) =
      new DenseVector(Seq.fill(dimensions)(0.0).updated(index, 1.0).toArray)

    def lg2(i: Int): Int = {
      val res = (log(i) / log(2)).intValue
      require(math.pow(2, res) == i)
      res
    }

    def fromUnitVector(uv: DenseVector[Double]): IndividualState = {
      val noOfQubits = lg2(uv.length)

      var qubits = Seq[Int]()
      var remaining = uv.toArray.indexOf(1.0)

      for (i <- 0 until noOfQubits) {
        qubits = remaining % 2 +: qubits
        remaining /= 2
      }

      IndividualState(1, qubits)
    }

    val baseStates = vector
      .toArray
      .toSeq
      .zipWithIndex
      .filter(_._1 != 0)
      .map{ case (d, index) =>
        val uv = unitVector(index, vector.length)
        fromUnitVector(uv) * d
      }

    if(baseStates.length == 1)
      baseStates.head
    else
      SuperposedState(baseStates)
  }

  implicit class StateCoefficient(coeff: Double) {
    def *(s: State): State = s match {
      case s: IndividualState =>
        s.copy(coefficient = s.coefficient * coeff)
      case ss: SuperposedState =>
        SuperposedState(ss.states.map(coeff * _).map(_.asInstanceOf[IndividualState]))
    }
  }
}
