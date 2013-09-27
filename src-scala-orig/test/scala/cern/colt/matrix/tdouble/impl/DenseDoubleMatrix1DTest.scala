package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix1D
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix1D
import cern.colt.matrix.tdouble.StrideMatrix1D
import cern.colt.matrix.tdouble.DoubleMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseDoubleMatrix1DTest(arg0: String) extends DoubleMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseMatrix1D(SIZE)
    B = new DenseMatrix1D(SIZE)
  }

  def testDct() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix1D].dct(true)
    A.asInstanceOf[DenseMatrix1D].idct(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testDst() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix1D].dst(true)
    A.asInstanceOf[DenseMatrix1D].idst(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testDht() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix1D].dht()
    A.asInstanceOf[DenseMatrix1D].idht(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testFft() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseMatrix1D].fft()
    A.asInstanceOf[DenseMatrix1D].ifft(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testGetFft() {
    val Acopy = A.copy()
    val ac = A.asInstanceOf[DenseMatrix1D].getFft
    ac.asInstanceOf[DenseDComplexMatrix1D].ifft(true)
    for (i <- 0 until A.size.toInt) {
      val elem = ac.getQuick(i)
      assertEquals(Acopy.getQuick(i), elem(0), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }

  def testGetIfft() {
    val Acopy = A.copy()
    val ac = A.asInstanceOf[DenseMatrix1D].getIfft(true)
    ac.asInstanceOf[DenseDComplexMatrix1D].fft()
    for (i <- 0 until A.size.toInt) {
      val elem = ac.getQuick(i)
      assertEquals(Acopy.getQuick(i), elem(0), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }
}
