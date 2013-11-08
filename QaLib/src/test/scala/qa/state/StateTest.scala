package qa.state

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import qa.matrix._
import qa.util._
import scala.math.sqrt

@RunWith(classOf[JUnitRunner])
class StateTest extends FlatSpec with ShouldMatchers {

  "A state" should "be easily creatable" in {
    val easilyCreatedState = State(0, 1)
    val leaf = easilyCreatedState.asInstanceOf[LeafState]

    assert(1 == leaf.coefficient)
    assert(new LeafState(1, List(0, 1)) == leaf)
  }

  it should "be multipliable to a double value as its coefficient" in {
    val multipliedState = (0.3 * State(0)).asInstanceOf[LeafState]

    assert(0.3 == multipliedState.coefficient)
  }

  it should "result in a LeafState if adding ones with the same qubits" in {
    val state = State(0)
    val halfState = 0.5 * state

    (halfState + halfState) should equal(state)
    (halfState + halfState).isInstanceOf[LeafState] should be(true)
  }

  it should "result in a CompositeState if adding ones with differing qubits" in {
    val stateFirst = State(0).asInstanceOf[LeafState]
    val stateSecond = State(1).asInstanceOf[LeafState]

    val compState = (stateFirst + stateSecond).asInstanceOf[CompositeState]
    compState.states.contains(stateFirst) should be(true)
    compState.states.contains(stateSecond) should be(true)
  }

  it should "support subtracting as adding it negated" in {
    val state = State(0)
    val halfState = 0.5 * state

    halfState should equal(state - halfState)
  }

  "There" should "be DSL-like syntax" in {
    0.5 * State(0, 1) + 0.5 * State(0, 1) should equal(State(0, 1))
  }

  "A state" should "be constructible from a Vector" in {
    val s = State.fromVector(qa.matrix.Vector(1, 0))
    s should equal(State(0))

    val s3 = State.fromVector(UnitVector(8, 2))
    s3 should equal(State(0, 1, 0))
  }

  it should "be combinable with another State" in {
    val (q0, q1, q01) = (State(0), State(1), State(0, 1))
    q0 tensor q1 should equal(q01)
  }

  it should "give (illegal) information about one of its qubits" in {
    val s = 0.5 * (State(0, 0) + State(0, 1) + State(1, 0) + State(1, 1))
    val expected = OS2 * (State(0) + State(1))
    
    s.inspectQubit(0) should equal(expected)
  }

}
