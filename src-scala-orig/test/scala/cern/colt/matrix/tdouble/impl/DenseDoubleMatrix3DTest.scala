package cern.colt.matrix.tdouble.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix3D
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix3D
import cern.colt.matrix.tdouble.DoubleMatrix3D
import cern.colt.matrix.tdouble.DoubleMatrix3DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseDoubleMatrix3DTest(arg0: String) extends DoubleMatrix3DTest(arg0) {

  protected def createMatrices() {
    A = new DenseDoubleMatrix3D(NSLICES, NROWS, NCOLUMNS)
    B = new DenseDoubleMatrix3D(NSLICES, NROWS, NCOLUMNS)
  }

  def testDct3() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDoubleMatrix3D].dct3(true)
    A.asInstanceOf[DenseDoubleMatrix3D].idct3(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(Acopy.getQuick(s, r, c) - A.getQuick(s, r, c)), TOL)
    }
  }

  def testDst3() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDoubleMatrix3D].dst3(true)
    A.asInstanceOf[DenseDoubleMatrix3D].idst3(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(Acopy.getQuick(s, r, c) - A.getQuick(s, r, c)), TOL)
    }
  }

  def testDht3() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDoubleMatrix3D].dht3()
    A.asInstanceOf[DenseDoubleMatrix3D].idht3(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(Acopy.getQuick(s, r, c) - A.getQuick(s, r, c)), TOL)
    }
  }

  def testFft3() {
    val nslices = 16
    val nrows = 32
    val ncolumns = 64
    val A = new DenseDoubleMatrix3D(nslices, nrows, ncolumns)
    val Acopy = A.copy()
    A.asInstanceOf[DenseDoubleMatrix3D].fft3()
    A.asInstanceOf[DenseDoubleMatrix3D].ifft3(true)
    for (s <- 0 until nslices; r <- 0 until nrows; c <- 0 until ncolumns) {
      assertEquals(0, Math.abs(Acopy.getQuick(s, r, c) - A.getQuick(s, r, c)), TOL)
    }
  }

  def testDct2Slices() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDoubleMatrix3D].dct2Slices(true)
    A.asInstanceOf[DenseDoubleMatrix3D].idct2Slices(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(Acopy.getQuick(s, r, c) - A.getQuick(s, r, c)), TOL)
    }
  }

  def testDst2Slices() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDoubleMatrix3D].dst2Slices(true)
    A.asInstanceOf[DenseDoubleMatrix3D].idst2Slices(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(Acopy.getQuick(s, r, c) - A.getQuick(s, r, c)), TOL)
    }
  }

  def testDft2Slices() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseDoubleMatrix3D].dht2Slices()
    A.asInstanceOf[DenseDoubleMatrix3D].idht2Slices(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      assertEquals(0, Math.abs(Acopy.getQuick(s, r, c) - A.getQuick(s, r, c)), TOL)
    }
  }

  def testGetFft3() {
    val Ac = A.asInstanceOf[DenseDoubleMatrix3D].getFft3
    Ac.asInstanceOf[DenseDComplexMatrix3D].ifft3(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = Ac.getQuick(s, r, c)
      assertEquals(A.getQuick(s, r, c), elem(0), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }

  def testGetIfft3() {
    val Ac = A.asInstanceOf[DenseDoubleMatrix3D].getIfft3(true)
    Ac.asInstanceOf[DenseDComplexMatrix3D].fft3()
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = Ac.getQuick(s, r, c)
      assertEquals(A.getQuick(s, r, c), elem(0), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }

  def testGetFft2Slices() {
    val Ac = A.asInstanceOf[DenseDoubleMatrix3D].getFft2Slices
    Ac.asInstanceOf[DenseDComplexMatrix3D].ifft2Slices(true)
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = Ac.getQuick(s, r, c)
      assertEquals(0, Math.abs(A.getQuick(s, r, c) - elem(0)), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }

  def testGetIfft2Slices() {
    val Ac = A.asInstanceOf[DenseDoubleMatrix3D].getIfft2Slices(true)
    Ac.asInstanceOf[DenseDComplexMatrix3D].fft2Slices()
    for (s <- 0 until A.slices(); r <- 0 until A.rows(); c <- 0 until A.columns()) {
      val elem = Ac.getQuick(s, r, c)
      assertEquals(0, Math.abs(A.getQuick(s, r, c) - elem(0)), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }
}
