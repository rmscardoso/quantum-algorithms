package qa.operator

import org.scalatest._
import qa.matrix._
import qa.operator.CNotTransitions._

class ControlledOperatorTest extends FlatSpec with Matchers {
  
  "I" can "check if all controlled bits equal 1 for a State" in {
    ControlledOperator.shouldApplyOperator(List(0, 0), List(0)) should be(false)
    ControlledOperator.shouldApplyOperator(List(0, 1), List(0)) should be(false)
    ControlledOperator.shouldApplyOperator(List(1, 0), List(0)) should be(true)
    ControlledOperator.shouldApplyOperator(List(1, 1), List(0)) should be(true)
    ControlledOperator.shouldApplyOperator(List(0, 0), List(1)) should be(false)
    ControlledOperator.shouldApplyOperator(List(0, 1), List(1)) should be(true)
    ControlledOperator.shouldApplyOperator(List(1, 0), List(1)) should be(false)
    ControlledOperator.shouldApplyOperator(List(1, 1), List(1)) should be(true)
  }
 
  "A Controlled-Operator" should "be created with a 'normal' Operator" in {
    object LocalCNot extends ControlledOperator(Not)
  }
  
  "The controlled operator" should "give the relevant matrix part of involved qubits" in {
    val cNotMatrix = Matrix((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 0, 1), (0, 0, 1, 0))
    CNot.involvedQubitsMatrix(List(0), 1) should equal(cNotMatrix)
  }
  
  "CNot" should "handle all 2 qubit combinations" in {
    testTransitions(List(0), 1)
    testTransitions(List(1), 0)
  }
  
  it should "handle all 3 qubit combinations" in {
    testTransitions(List(0), 2)
    testTransitions(List(1), 2)
    testTransitions(List(2), 0)
    testTransitions(List(2), 1)
    testTransitions(List(0, 1), 2)
    testTransitions(List(0, 2), 1)
    testTransitions(List(1, 2), 0)
    testTransitions(List(2, 1), 0)
  }
  
  it should "handle some 4 qubit combinations (i trust the rest" in {
    testTransitions(List(0, 2), 3)
  }
  
  def testTransitions(controlling: List[Int], controlled: Int) {
    val cn = CNot(controlling: _*)(controlled) _
    for((from, to) <- transitions(controlling, controlled)) {
      cn(from) should equal(to)
    }
  }
  
}