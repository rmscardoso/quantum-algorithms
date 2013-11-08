package qa.operator

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import qa.util._
import qa.operator._
import qa.state._

@RunWith(classOf[JUnitRunner])
class OperatorTest extends FlatSpec with ShouldMatchers {

  "An operator" should "be able to apply a Single-Qubit-State" in {
    val s = State(0)

    val sH = OS2 * State(0) + OS2 * State(1)
    H(s) should equal(sH)
  }

  it should "be able to apply a Multi-Qubit-State" in {
    val s = State(0, 0)

    val sH = 0.5 * State(0, 0) + 0.5 * State(0, 1) + 0.5 * State(1, 0) + 0.5 * State(1, 1)
    H(s) should equal(sH)
  }

  it should "be able to apply a subset of a State's qubits" in {
    val s = State(0, 0, 0)
    val sH = OS2 * State(0, 0, 0) + OS2 * State(0, 1, 0)

    H(1)(s) should equal(sH)
    H(0, 1, 2)(s) should equal(H(s))
    H(0 to 2)(s) should equal(H(s))
  }
  
  it should "apply Not operators on indexed qubit" in {
    val (s00, s01, s10, s11) = (State(0, 0), State(0, 1), State(1, 0), State(1, 1))
    Not(0)(s00) should equal(s10)
    Not(0)(s01) should equal(s11)
    Not(0)(s10) should equal(s00)
    Not(0)(s11) should equal(s01)
    Not(1)(s00) should equal(s01)
    Not(1)(s01) should equal(s00)
    Not(1)(s10) should equal(s11)
    Not(1)(s11) should equal(s10)
  }

}