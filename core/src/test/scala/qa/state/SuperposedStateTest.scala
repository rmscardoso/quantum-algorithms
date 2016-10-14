package qa.state

import breeze.linalg.DenseVector
import org.scalacheck.Gen
import qa.TestCase
import qa.state.State._

class SuperposedStateTest extends TestCase {
  import SuperposedStateTest._

  "Multiplying a superposed State by a number" should "multiply its coefficient" in {
    val original = 0.5 * State(0, 1) + 0.5 * State(1, 0)
    val multiplied = 0.5 * original
    val expected = 0.25 * State(0, 1) + 0.25 * State(1, 0)

    multiplied shouldEqual expected
  }

  "Adding a base State to a superposed State" should "add the coefficients of the corresponding base State" in {
    val first = State(0)
    val second = State(1)
    val third = State(1)
    val firstAndSecond = first + second
    val allThree = firstAndSecond + third
    val expected = State(0) + 2 * State(1)

    allThree shouldEqual expected
  }

  "A superposed State" should "be convertible to a Vector" in {
    val s = 0.5 * State(0) + 0.3 * State(1)
    s.toVector shouldEqual DenseVector(Array(0.5, 0.3))
  }

  it should "be creatable from a Vector" in {
    val s = State.fromVector(DenseVector(Array(0.0, 0.5, 0.3, 0.0)))
    s shouldEqual 0.5 * State(0, 1) + 0.3 * State(1, 0)
  }

  "Superposed States" should "have commutative addition" in {
    for(n <- 1 to 10) {
      forAll(superposedStateGen(n), superposedStateGen(n)) { (l: SuperposedState, r: SuperposedState) =>
        l + r shouldEqual r + l
      }
    }
  }

  they should "have commutative multiplication" in {
    for(n <- 1 to 10) {
      forAll(superposedStateGen(n), Gen.choose(-10.0, 10.0)) { (s: SuperposedState, d: Double) =>
        s * d shouldEqual d * s
      }
    }
  }
}

object SuperposedStateTest {
  import BaseStateTest.baseStates

  val superposedStateGen = Map[Int, Gen[SuperposedState]]().withDefault { numberOfQubits =>
    for {
      n <- Gen.choose(2, 10)
      baseStates <- Gen.listOfN(n, baseStates(numberOfQubits))
    } yield SuperposedState(baseStates)
  }
}
