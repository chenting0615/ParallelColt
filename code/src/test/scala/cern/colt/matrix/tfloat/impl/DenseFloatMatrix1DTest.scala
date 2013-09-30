package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix1D
import cern.colt.matrix.tfcomplex.impl.DenseFComplexMatrix1D
import cern.colt.matrix.tfloat.FloatMatrix1D
import cern.colt.matrix.tfloat.FloatMatrix1DTest
//remove if not needed
import scala.collection.JavaConversions._

class DenseFloatMatrix1DTest(arg0: String) extends FloatMatrix1DTest(arg0) {

  protected def createMatrices() {
    A = new DenseFloatMatrix1D(SIZE)
    B = new DenseFloatMatrix1D(SIZE)
  }

  def testDct() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix1D].dct(true)
    A.asInstanceOf[DenseFloatMatrix1D].idct(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testDst() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix1D].dst(true)
    A.asInstanceOf[DenseFloatMatrix1D].idst(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testDht() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix1D].dht()
    A.asInstanceOf[DenseFloatMatrix1D].idht(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testFft() {
    val Acopy = A.copy()
    A.asInstanceOf[DenseFloatMatrix1D].fft()
    A.asInstanceOf[DenseFloatMatrix1D].ifft(true)
    for (i <- 0 until A.size.toInt) {
      assertEquals(Acopy.getQuick(i), A.getQuick(i), TOL)
    }
  }

  def testGetFft() {
    val Acopy = A.copy()
    val ac = A.asInstanceOf[DenseFloatMatrix1D].getFft
    ac.asInstanceOf[DenseFComplexMatrix1D].ifft(true)
    for (i <- 0 until A.size.toInt) {
      val elem = ac.getQuick(i)
      assertEquals(Acopy.getQuick(i), elem(0), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }

  def testGetIfft() {
    val Acopy = A.copy()
    val ac = A.asInstanceOf[DenseFloatMatrix1D].getIfft(true)
    ac.asInstanceOf[DenseFComplexMatrix1D].fft()
    for (i <- 0 until A.size.toInt) {
      val elem = ac.getQuick(i)
      assertEquals(Acopy.getQuick(i), elem(0), TOL)
      assertEquals(0, elem(1), TOL)
    }
  }
}
