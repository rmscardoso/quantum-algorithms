package qa.state

import breeze.linalg.DenseVector
import org.scalacheck.Gen
import qa.TestCase
import qa.state.State._

class BaseStateTest extends TestCase {
  import BaseStateTest._
  import StateTest._

  val smallNumbersOfQubits = 1 to  2
  val allNumbersOfQubits   = 1 to 10

  "BaseState multiplication" should "multiply the BaseState's coefficient" in {
    for(n <- allNumbersOfQubits) {
      forAll(baseStates(n), realNumbers) { (s: BaseState, d: Double) =>
        val multiplied = d * s
        val expected = BaseState(d * s.coefficient, s.qubits)

        multiplied shouldEqual expected
      }
    }
  }

  it should "be commutative" in {
    for(n <- allNumbersOfQubits) {
      forAll(baseStates(n), Gen.choose(-10.0, 10.0)) { (s: BaseState, d: Double) =>
        s * d shouldEqual d * s
      }
    }
  }

  "BaseState addition" should "create another BaseState for the same Qubits while adding coefficients" in {
    for(n <- smallNumbersOfQubits) {
      forAll(baseStates(n), baseStates(n)) { (l: BaseState, r: BaseState) =>
        whenever(l.qubits == r.qubits) {
          l + r match {
            case sum: BaseState =>
              sum.qubits shouldEqual l.qubits
              sum.coefficient shouldEqual l.coefficient + r.coefficient

            case _ => fail()
          }
        }
      }
    }
  }

  it should "create a SuperposedState for different Qubits, containing both BaseStates" in {
    for(n <- smallNumbersOfQubits) {
      forAll(baseStates(n), baseStates(n)) { (l: BaseState, r: BaseState) =>
        whenever(l.qubits != r.qubits) {
          l + r match {
            case sum: SuperposedState =>
              sum.states.size shouldBe 2
              sum.states should contain(l)
              sum.states should contain(r)

            case _ => fail()
          }
        }
      }
    }
  }

  it should "be commutative" in {
    for(n <- allNumbersOfQubits) {
      forAll(baseStates(n), baseStates(n)) { (l: BaseState, r: BaseState) =>
        l + r shouldEqual r + l
      }
    }
  }

  "A BaseState" should "be convertible to a Vector" in {
    BaseState(0.5, Seq(0)).toVector shouldEqual DenseVector[Double](0.5, 0)

    BaseState(0.5, Seq(1, 0)).toVector shouldEqual DenseVector[Double](0, 0, 0.5, 0)
  }

  it should "be creatable from a Vector" in {
    for(n <- allNumbersOfQubits) {
      forAll(baseStates(n)) { s: BaseState =>
        val v = s.toVector
        State.fromVector(v) shouldEqual s
      }
    }
  }
}

object BaseStateTest {
  import StateTest.qubits

  val baseStates = Map[Int, Gen[BaseState]]().withDefault { numberOfQubits =>
    for {
      coefficient <- Gen.choose(-1.0, 1.0)
      qubits <- qubits(numberOfQubits)
    } yield BaseState(coefficient, qubits)
  }
}
