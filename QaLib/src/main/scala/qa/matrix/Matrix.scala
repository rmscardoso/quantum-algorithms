package qa.matrix

import scala.collection.mutable.{ ListBuffer => MutableList }
import qa.util._
import Matrix._

class Matrix (val rep: List[List[Double]]) extends DoubleUtils {

  validateMatrix(rep)

  def rows: Int =
    rep.size

  def columns: Int =
    rep(0).size

  def apply(n: Int): List[Double] =
    rep(n)

  def *(d: Double): Matrix = {
    val newRep: List[List[Double]] =
      for (row <- rep) yield {
        for (entry <- row)
          yield d * entry
      }

    new Matrix(newRep)
  }

  def *(other: Matrix): Matrix = {
    require(this.columns == other.rows)

    val res = Array.fill(this.columns, other.columns)(0D)
    val myArr = listOfListsToTwoDimArray(rep)
    val otherArr = listOfListsToTwoDimArray(other.rep)

    for (
      row <- 0 until this.rows;
      col <- 0 until other.columns;
      i <- 0 until this.columns
    ) {
      res(row)(col) += myArr(row)(i) * otherArr(i)(col)
    }

    new Matrix(twoDimArrayToListOfLists(res))
  }

  def *(other: Vector): Vector =
    (this * other.asInstanceOf[Matrix]).toVector

  def +(other: Matrix): Matrix = {
    require(rows == other.rows, "Needs equal number of rows")
    require(columns == other.columns, "Needs equal number of columns")

    val newRep: List[List[Double]] =
      for ((myRow, otherRow) <- rep zip other.rep) yield {
        for ((entry, otherEntry) <- myRow zip otherRow)
          yield entry + otherEntry
      }

    new Matrix(newRep)
  }

  def transposed: Matrix = {
    new Matrix(rep transpose)
  }

  def tensor(other: Matrix): Matrix = {
    val res = Array.ofDim[Double](this.rows * other.rows, this.columns * other.columns)

    for {
      myRow <- 0 until rows
      myCol <- 0 until columns
      otherRow <- 0 until other.rows
      otherCol <- 0 until other.columns
    } {
      res(myRow * other.rows + otherRow)(myCol * other.columns + otherCol) =
        rep(myRow)(myCol) * other.rep(otherRow)(otherCol)
    }

    new Matrix(twoDimArrayToListOfLists(res))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: UnitVector => listsOfDoubleListsNearlyEqual(rep, other.rep)
      case other: Vector => listsOfDoubleListsNearlyEqual(rep, other.rep)
      case other: Matrix => listsOfDoubleListsNearlyEqual(rep, other.rep)
      case _ => false
    }
  }

  override def toString: String = {
    rep map (_ mkString ("\t")) mkString ("Matrix(\n", "\n", ")")
  }

  def toVector: Vector =
    if (1 == columns)
      new Vector(rep)
    else
      sys.error(this + " is not a Vector")

}

object Matrix {

  def apply(matrix: List[Double]*): Matrix =
    new Matrix(matrix.toList)

  private def validateMatrix[T](rep: List[List[T]]) {
    require(!rep.isEmpty)
    require(!rep.head.isEmpty)

    val size = rep.head.size
    for (subList <- rep.tail)
      require(size == subList.size)
  }

  private def twoDimArrayToListOfLists(arr: Array[Array[Double]]): List[List[Double]] =
    (arr map (_ toList)) toList

  private def listOfListsToTwoDimArray(lol: List[List[Double]]): Array[Array[Double]] =
    (lol map (_ toArray)) toArray

}