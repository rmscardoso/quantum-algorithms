package qa.util

import math.{ sqrt, pow }
import qa.util._

trait DoubleUtils {

  protected def listsOfDoubleListsNearlyEqual(l: List[List[Double]], r: List[List[Double]]): Boolean = {
    require(l.size != 0 && l.size == r.size)

    l match {
      case head :: Nil =>
        doubleListsNearlyEqual(head, r.head)
      case head :: tail =>
        doubleListsNearlyEqual(head, r.head) && listsOfDoubleListsNearlyEqual(tail, r.tail)
      case _ =>
        sys.error("unreachable")
    }
  }

  protected def doubleListsNearlyEqual(l: List[Double], r: List[Double]): Boolean = {
    require(l.size != 0 && l.size == r.size)

    l match {
      case head :: Nil => head ~= r.head
      case head :: tail => (head ~= r.head) && doubleListsNearlyEqual(tail, r.tail)
      case _ => sys.error("unreachable")
    }
  }

}