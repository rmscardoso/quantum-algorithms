package qa.state

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito._
import qa.state.State._
import qa.util._

import scala.util.Random

class CompositeStateTest extends FlatSpec with Matchers with MockitoSugar {

  "A CompositeState" should "accept only substates with same number of qubits" in {
    consolidate(List(State(0), State(1)))
    consolidate(List(State(0, 0), State(0, 1), State(1, 0), State(1, 1)))

    intercept[IllegalArgumentException] {
      consolidate(List(State(0), State(0, 0)))
    }
  }

  it should "apply multiply to all included LeafStates" in {
    val firstState = new LeafState(0.3, List(0))
    val secondState = new LeafState(0.4, List(1))
    val mul = 2D
    val compState = (consolidate(List(firstState, secondState)) * mul).asInstanceOf[CompositeState]

    compState.states should (
      contain(new LeafState(mul * firstState.coefficient, firstState.qubits))
      and contain(new LeafState(mul * secondState.coefficient, secondState.qubits)))
  }

  it should "provide an equals method" in {
    val s1 = new LeafState(0.3, List(0))
    val s2 = new LeafState(0.4, List(1))

    val s3 = (2 * s1).asInstanceOf[LeafState]
    val s4 = (2 * s2).asInstanceOf[LeafState]

    val comp1 = new CompositeState(List(s1, s2))
    val comp2 = new CompositeState(List(s3, s4))

    (comp1 == comp2) should be(false)
    (2 * comp1 == comp2) should be(true)
  }

  it should "be convertible to a vector" in {
    (State(0) + State(1)).toVector should equal(qa.matrix.Vector(1D, 1D))
  }

  it should "be measurable" in {
    val rng = mock[Random]
    State.rng = rng

    val s = OS2 * State(0) + OS2 * State(1)

    when(rng.nextDouble).thenReturn(OS2 * OS2)
    s.measure should equal(State(0))

    when(rng.nextDouble).thenReturn(OS2 * OS2 - 0.1)
    s.measure should equal(State(0))

    when(rng.nextDouble).thenReturn(OS2 * OS2 + 0.1)
    s.measure should equal(State(1))

    when(rng.nextDouble).thenReturn(1D)
    s.measure should equal(State(1))
  }

  it should "contain LeafStates ordered by the qubits" in {
    val s1 = new LeafState(1, List(0, 1))
    val s2 = new LeafState(1, List(1, 0))
    val comp = new CompositeState(List(s2, s1))

    comp.states should equal(List(s1, s2))
    comp.states should not equal (List(s2, s1))
  }

  "A partial measurement" should "remove all now invalid states" in {
    val rng = mock[Random]
    State.rng = rng
  }

  it should "be partially measurable" in {
    val rng = mock[Random]
    State.rng = rng

    val s00 = new LeafState(0.5, List(0, 0))
    val s01 = new LeafState(0.5, List(0, 1))
    val s10 = new LeafState(0.5, List(1, 0))
    val s11 = new LeafState(0.5, List(1, 1))
    val comp = new CompositeState(List(s00, s10, s01, s11))

    when(rng.nextDouble).thenReturn(0D)
    val (measured0_0, remaining0_0) = comp.measureSingleQubit(0)
    measured0_0 should equal(0)
    checkStateHasAllSubStates(remaining0_0, List(State(0, 0), State(0, 1)))
    
    when(rng.nextDouble).thenReturn(1D)
    val (measured0_1, remaining0_1) = comp.measureSingleQubit(0)
    measured0_1 should equal(1)
    checkStateHasAllSubStates(remaining0_1, List(State(1, 0), State(1, 1)))
    
    when(rng.nextDouble).thenReturn(0D)
    val (measured1_0, remaining1_0) = comp.measureSingleQubit(1)
    measured1_0 should equal(0)
    checkStateHasAllSubStates(remaining1_0, List(State(0, 0), State(1, 0)))
    
    when(rng.nextDouble).thenReturn(1D)
    val (measured1_1, remaining1_1) = comp.measureSingleQubit(1)
    measured1_1 should equal(1)
    checkStateHasAllSubStates(remaining1_1, List(State(0, 1), State(1, 1)))
  }

  def checkStateHasAllSubStates(s: State, l: List[State]) {
    val presentSubStatesQubits: List[List[Int]] = s.statesList map { _.asInstanceOf[LeafState].qubits }
    val expectedSubStatesQubits: List[List[Int]] = l map { _.asInstanceOf[LeafState].qubits }
    
    presentSubStatesQubits.size should equal(expectedSubStatesQubits.size)
    presentSubStatesQubits forall { expectedSubStatesQubits contains _ } should be(true)
  }

  "A partial measurement" should "recalculate coefficients" in {
    val rng = mock[Random]
    State.rng = rng

    val s10 = new LeafState(OS2, List(1, 0))
    val s11 = new LeafState(OS2, List(1, 1))
    val comp = new CompositeState(List(s10, s11))

    (1D ~= comp.measureSingleQubit(1)._2.asInstanceOf[LeafState].coefficient) should be(true)
  }

  "toString" should "give a concatenation of LeafState's toString" in {
    val s1 = new LeafState(0.3, List(0, 1))
    val s2 = new LeafState(-0.55, List(1, 0))
    val comp = new CompositeState(List(s1, s2))
    comp.toString() should equal(s1.toString() + " " + s2.toString())
  }

}