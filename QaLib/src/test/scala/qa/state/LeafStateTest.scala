package qa.state

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import qa.matrix._
import scala.util.Sorting

@RunWith(classOf[JUnitRunner])
class LeafStateTest extends FlatSpec with ShouldMatchers {

  "A LeafState" should "be multiplyable by coefficients" in {
    val coefficient = 0.3

    val baseState = new LeafState(1, List(0))

    val firstState = baseState * coefficient
    firstState.coefficient should equal(coefficient)

    val secondState = (baseState * coefficient) * coefficient
    secondState.coefficient should equal(coefficient * coefficient)
  }

  it should "have at least one qubit" in {
    intercept[IllegalArgumentException] {
      new LeafState(1, List())
    }
  }

  it should "give itself in a list" in {
    val state = new LeafState(1, List(0))
    val stateList = state.statesList

    stateList.length should be(1)
    stateList.contains(state) should be(true)
  }

  it should "accept only 0s and 1s as qubits" in {
    new LeafState(1, List(0))
    new LeafState(1, List(1))
    new LeafState(1, List(0, 1))

    intercept[IllegalArgumentException] {
      new LeafState(1, List(-1))
    }

    intercept[IllegalArgumentException] {
      new LeafState(1, List(2))
    }

    intercept[IllegalArgumentException] {
      new LeafState(1, List(0, 2))
    }
  }

  it should "equal another LeafState with same coefficient and qubits" in {
    val zeros = List(0, 0)
    val coefficient = 0.3

    val state = new LeafState(coefficient, zeros)

    state should (
      equal(new LeafState(coefficient, zeros))
      and equal(new LeafState(0.3, List(0, 0))))

    state should (
      not equal (new LeafState(coefficient * coefficient, zeros))
      and not equal (new LeafState(coefficient, List(0)))
      and not equal (new LeafState(coefficient, List(0, 1)))
      and not equal (new LeafState(coefficient, List(0, 0, 0))))
  }

  it should "give a CompositeState when adding another LeafState with differing qubits" in {
    val firstState = new LeafState(0.3, List(0))
    val secondState = new LeafState(0.3, List(1))
    val compState = (firstState + secondState).asInstanceOf[CompositeState]

    compState.states.contains(firstState) should be(true)
    compState.states.contains(secondState) should be(true)
  }

  it should "be normalized if and only if its coefficient is 1 or -1" in {
    val baseState = new LeafState(1, List(0))

    baseState.isNormalized should be(true)
    (-1 * baseState).isNormalized should be(true)
    (0 * baseState).isNormalized should be(false)
    (0.3 * baseState).isNormalized should be(false)
  }

  it should "provide a readable expression in toString" in {
    new LeafState(1, List(0, 1)).toString should equal("+ 1.0|0, 1>")
    new LeafState(0.3, List(0, 1)).toString should equal("+ 0.3|0, 1>")
    new LeafState(-1, List(0)).toString should equal("- 1.0|0>")
    new LeafState(-0.777, List(0, 1, 1, 0)).toString should equal("- 0.777|0, 1, 1, 0>")
  }

  it should "be convertible to a vector" in {
    State(0).toVector should equal(UnitVector(2, 0))

    State(0, 1).toVector should equal(UnitVector(4, 1))

    (0.3 * State(0)).toVector should equal(0.3 * UnitVector(2, 0))
  }

  it should "be measurable" in {
    val s0 = State(0)
    s0.measure should equal(s0)

    val s1 = State(0, 1, 0)
    s1.measure should equal(s1)
  }

  it should "be able to calculate its qubits bit-value" in {
    new LeafState(1D, List(1, 0, 1)).calcBitValue() should equal(5)
    new LeafState(1D, List(0, 1, 1, 0)).calcBitValue() should equal(6)
  }

  it should "be sortable" in {
    val s0 = new LeafState(1D, List(0, 0))
    val s1 = new LeafState(1D, List(0, 1))
    val s2 = new LeafState(1D, List(1, 0))
    val s3 = new LeafState(1D, List(1, 1))

    val s = List(s2, s1, s3, s0)

    s.sorted(LeafStateOrdering) should equal(List(s0, s1, s2, s3))
    s.sorted(LeafStateOrdering) should not equal (List(s2, s1, s3, s0))
  }

  it should "be partially measurable" in {
    val s = new LeafState(1D, List(0, 1))
    
    val (measured0, remaining0) = s.measureSingleQubit(0)
    measured0 should equal(0)
    remaining0 should equal(s)
    
    val (measured1, remaining1) = s.measureSingleQubit(1)
    measured1 should equal(1)
    remaining1 should equal(s)
  }

}