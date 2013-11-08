package qa.matrix

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FlatSpec with ShouldMatchers {

  "A Matrix" should "be creatable by some Lists" in {
    val (a, b, c, d) = (0, 1, 1, 0)

    val matrix = Matrix(List(a, b), List(c, d));
    a should equal(matrix.rep(0)(0))
    b should equal(matrix.rep(0)(1))
    c should equal(matrix.rep(1)(0))
    d should equal(matrix.rep(1)(1))
  }

  it should "provide an equals method" in {
    val (a, b, c, d) = (1, 2, 3, 4)
    val (e, f, g, h) = (2, 4, 6, 8)

    val m1 = Matrix((a, b), (c, d))
    val m2 = Matrix(List(e, f), List(g, h))

    m1 == m2 should be(false)
    2 * m1 == m2 should be(true)
  }

  it should "be creatable by touples" in {
    Matrix(
      (0, 1),
      (1, 0)) should equal(Matrix(List(0D, 1D), List(1D, 0D)))
  }

  it should "only accept lists of same size" in {
    Matrix(List(0, 1))
    Matrix(List(0), List(1))

    intercept[IllegalArgumentException] {
      Matrix(List(0), List(0, 1))
    }
  }

  it should "only be creatable with non-empty Lists" in {
    intercept[IllegalArgumentException] {
      Matrix()
    }

    intercept[IllegalArgumentException] {
      Matrix(Nil)
    }
  }

  it should "give easy access to its representation" in {
    val (a, b, c, d) = (0, 1, 1, 0)
    val (firstRow, secondRow) = (List(a, b), List(c, d))

    val matrix = Matrix(firstRow, secondRow);
    firstRow should equal(matrix(0))
    a should equal(matrix(0)(0))
    b should equal(matrix(0)(1))
    secondRow should equal(matrix(1))
    c should equal(matrix(1)(0))
    d should equal(matrix(1)(1))
  }

  it should "accept different castable types" in {
    val (a, b, c, d) = (0, 1, 1, 0.1)
    val (firstRow, secondRow) = (List(a, b), List(c, d))

    val matrix = Matrix(firstRow, secondRow)
  }

  it should "be multipliable by a numeric value" in {
    val (a, b, c, d) = (1, 2, 3, 4)
    val m = Matrix(List(a, b), List(c, d))

    val multi1 = 2.5
    val m1 = multi1 * m
    m1(0)(0) should equal(multi1 * a)
    m1(0)(1) should equal(multi1 * b)
    m1(1)(0) should equal(multi1 * c)
    m1(1)(1) should equal(multi1 * d)

    val multi2 = -2
    val m2 = multi2 * m
    m2(0)(0) should equal(multi2 * a)
    m2(0)(1) should equal(multi2 * b)
    m2(1)(0) should equal(multi2 * c)
    m2(1)(1) should equal(multi2 * d)
  }

  it should "be multipliable by another Matrix" in {
    val (a, b, c, d) = (1, 2, 3, 4)
    val (e, f) = (5, 6)

    val m1 = Matrix(List(a, b), List(c, d))
    val m2 = Matrix(List(e), List(f))

    val expected = Matrix(List(a * e + b * f), List(c * e + d * f))
    m1 * m2 should equal(expected)
  }

  it should "be addable to another matrix of same NxM type" in {
    val (a, b, c, d) = (1, 2, 3, 4)
    val m1 = Matrix(List(a, b), List(c, d))
    val (e, f, g, h) = (5, 6, 7, 8)
    val m2 = Matrix(List(e, f), List(g, h))

    val m3 = m1 + m2
    m3(0)(0) should equal(a + e)
    m3(0)(1) should equal(b + f)
    m3(1)(0) should equal(c + g)
    m3(1)(1) should equal(d + h)
  }

  it should "be transposable" in {
    val (a, b) = (1, 2)
    val m = Matrix(List(a, b))

    val mT = m transposed

    m.rows should equal(mT.columns)
    m.columns should equal(mT.rows)

    m(0)(0) should equal(mT(0)(0))
    m(0)(1) should equal(mT(1)(0))
  }

  it should "provide a tensor product method" in {
    val (a, b, c, d) = (1, 2, 3, 4)
    val (e, f) = (5, 6)

    val m1 = Matrix(List(a, b), List(c, d))
    val m2 = Matrix(List(e), List(f))

    val expected = Matrix(
      List(a * e, b * e),
      List(a * f, b * f),
      List(c * e, d * e),
      List(c * f, d * f))

    m1 tensor m2 should equal(expected)
  }

}