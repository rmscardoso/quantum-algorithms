package qa

import qa.matrix._
import qa.util._

package object operator {

  object I extends Operator(Matrix(
    (1, 0),
    (0, 1)))

  object H extends Operator(Matrix(
    (OS2, OS2),
    (OS2, -OS2)))

  object Not extends Operator(Matrix(
    (0, 1),
    (1, 0)))

  object CNot extends ControlledOperator(Not)

}