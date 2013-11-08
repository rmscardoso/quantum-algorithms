import qa.state.State
import qa.operator._
import qa.matrix.Matrix
import scala.util.Random

object Teleporation extends App {

  val X = Not
  object Z extends Operator(Matrix(List(1, 0), List(0, -1)))

  // Alice has qubits q0 and q1
  // Bob has qubit q2
  // qubit q0 is the qubit we want to teleport
  // qubits q1 and q2 are in an entangled Bell State
  // in the end, q2 should be in the same state as q0 was in the beginning

  val q0 = randomOneQubitState
  println("State to teleport (q0): " + q0)

  val q12 = bell
  println("Bell state (q1 & q2): " + q12)

  val s0 = q0 tensor q12
  println("Starting with " + s0)

  val s1 = CNot(0)(1)(s0)
  val s2 = H(0)(s1)
  println("Before measurements: " + s2)

  val (m1, s3a) = s2.measureSingleQubit(0)
  println("measured q0: " + m1 + ", remaining: " + s3a)
  val (m2, s3b) = s3a.measureSingleQubit(1)
  println("measured q1: " + m2 + ", remaining: " + s3b)

  var s3 = s3b
  println("Before optional operations: " + s3)

  if (1 == m2)  {
   s3 = X(2)(s3)
   println("Applied X to q2: " + s3)
  }
  if (1 == m1) {
    s3 = Z(2)(s3)
    println("Applied Z to q2: " + s3)
  }
  
  println("Ending with " + s3)
  val q2 = s3.inspectQubit(2)
  
  println("Ending with " + s3)
  println("Teleported qubit(q2): " + q2)
  println("Qubits equal: " + (q0 == q2))

  def randomOneQubitState = {
    val rng = new Random
    val alpha = rng.nextDouble * (if (0.5 > rng.nextDouble) -1 else 1)
    val beta = math.sqrt(1D - alpha * alpha) * (if (0.5 > rng.nextDouble) -1 else 1)

    alpha * State(0) + beta * State(1)
  }

  def bell = {
    val os2 = 1D / math.sqrt(2)
    os2 * State(0, 0) + os2 * State(1, 1)
  }

}