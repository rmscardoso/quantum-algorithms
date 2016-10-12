package qa

import breeze.linalg.DenseVector
import org.scalacheck.Gen
import qa.State._

class StateTest extends TestCase {

  "Multiplying a State by a number" should "multiply its coefficient" in {
    val original = 0.5 * State(0, 1)
    val multiplied = 0.5 * original
    val expected = 0.25 * State(0, 1)

    multiplied shouldEqual expected
  }

  "Adding States" should "create another State" in {
    val first = sqrt2 * State(0)
    val second = sqrt2 * State(1)

    val sum = first + second
    sum.states should contain(first)
    sum.states should contain(second)
  }

  "Adding States with the same Qubit" should "add coefficients" in {
    val first = State(0)
    val second = State(0)
    val expected = 2 * State(0)

    val sum = first + second
    sum shouldEqual expected
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

  "A base State" should "be convertible to a Vector" in {
    val s = 0.5 * State(0, 1)
    s.toVector shouldEqual DenseVector(Array(0.0, 0.5))
  }

  "A superposed State" should "be convertible to a Vector" in {
    val s = 0.5 * State(0, 1) + 0.3 * State(1, 0)
    s.toVector shouldEqual DenseVector(Array(0.3, 0.5))
  }

  val qubitGen = Gen.oneOf(0, 1)

  val qubitsGen = Map[Int, Gen[Seq[Int]]]().withDefault { numberOfQubits =>
    Gen.listOfN(numberOfQubits, qubitGen)
  }

  val baseStateGen = Map[Int, Gen[BaseState]]().withDefault { numberOfQubits =>
    for {
      coefficient <- Gen.choose[Double](-1.0, 1.0)
      qubits <- qubitsGen(numberOfQubits)
    } yield BaseState(coefficient, qubits)
  }

  val superposedStateGen = Map[Int, Gen[SuperposedState]]().withDefault { numberOfQubits =>
    for {
      n <- Gen.choose(2, 10)
      baseStates <- Gen.listOfN(n, baseStateGen(numberOfQubits))
    } yield SuperposedState(baseStates)
  }

  "Base State addition" should "be commutative" in {
    forAll(baseStateGen(1), baseStateGen(1)) { (l: BaseState, r: BaseState) =>
      l + r shouldEqual r + l
    }

    forAll(baseStateGen(2), baseStateGen(2)) { (l: BaseState, r: BaseState) =>
      l + r shouldEqual r + l
    }
  }

  "Superposed State addition" should "be commutative" in {
    forAll(superposedStateGen(1), superposedStateGen(1)) { (l: SuperposedState, r: SuperposedState) =>
      l + r shouldEqual r + l
    }

    forAll(superposedStateGen(2), superposedStateGen(2)) { (l: SuperposedState, r: SuperposedState) =>
      l + r shouldEqual r + l
    }
  }

}
