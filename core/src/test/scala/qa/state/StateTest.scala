package qa.state

import org.scalacheck.{Arbitrary, Gen}

object StateTest {
  val realNumbers = Arbitrary.arbitrary[Double]

  private val baseStates = Gen.oneOf(0, 1)

  val qubits = Map[Int, Gen[Seq[Int]]]().withDefault { numberOfQubits =>
    Gen.listOfN(numberOfQubits, baseStates)
  }
}
