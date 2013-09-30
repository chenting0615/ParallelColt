package cern.colt.matrix.tfloat.algo.solver

import junit.framework.Test
import junit.framework.TestSuite
//remove if not needed
import scala.collection.JavaConversions._

object AllFloatMatrixSolverTests {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tfloat.algo.solvers tests")
    suite.addTestSuite(classOf[FloatCGTest])
    suite.addTestSuite(classOf[FloatCGDiagonalTest])
    suite.addTestSuite(classOf[FloatCGSSORTest])
    suite.addTestSuite(classOf[FloatCGILUTest])
    suite.addTestSuite(classOf[FloatCGICCTest])
    suite.addTestSuite(classOf[FloatCGILUTTest])
    suite.addTestSuite(classOf[FloatCGSTest])
    suite.addTestSuite(classOf[FloatCGSDiagonalTest])
    suite.addTestSuite(classOf[FloatCGSSSORTest])
    suite.addTestSuite(classOf[FloatCGSILUTest])
    suite.addTestSuite(classOf[FloatCGSICCTest])
    suite.addTestSuite(classOf[FloatCGSILUTTest])
    suite.addTestSuite(classOf[FloatQMRTest])
    suite.addTestSuite(classOf[FloatQMRDiagonalTest])
    suite.addTestSuite(classOf[FloatQMRSSORTest])
    suite.addTestSuite(classOf[FloatQMRILUTest])
    suite.addTestSuite(classOf[FloatQMRICCTest])
    suite.addTestSuite(classOf[FloatQMRILUTTest])
    suite.addTestSuite(classOf[FloatBiCGTest])
    suite.addTestSuite(classOf[FloatBiCGDiagonalTest])
    suite.addTestSuite(classOf[FloatBiCGSSORTest])
    suite.addTestSuite(classOf[FloatBiCGILUTest])
    suite.addTestSuite(classOf[FloatBiCGICCTest])
    suite.addTestSuite(classOf[FloatBiCGAMGTest])
    suite.addTestSuite(classOf[FloatBiCGILUTTest])
    suite.addTestSuite(classOf[FloatBiCGstabTest])
    suite.addTestSuite(classOf[FloatBiCGstabDiagonalTest])
    suite.addTestSuite(classOf[FloatBiCGstabSSORTest])
    suite.addTestSuite(classOf[FloatBiCGstabILUTest])
    suite.addTestSuite(classOf[FloatBiCGstabICCTest])
    suite.addTestSuite(classOf[FloatBiCGstabAMGTest])
    suite.addTestSuite(classOf[FloatBiCGstabILUTTest])
    suite.addTestSuite(classOf[FloatGMRESTest])
    suite.addTestSuite(classOf[FloatGMRESDiagonalTest])
    suite.addTestSuite(classOf[FloatGMRESSSORTest])
    suite.addTestSuite(classOf[FloatGMRESILUTest])
    suite.addTestSuite(classOf[FloatGMRESICCTest])
    suite.addTestSuite(classOf[FloatGMRESAMGTest])
    suite.addTestSuite(classOf[FloatGMRESILUTTest])
    suite.addTestSuite(classOf[FloatChebyshevTest])
    suite.addTestSuite(classOf[FloatChebyshevDiagonalTest])
    suite.addTestSuite(classOf[FloatChebyshevSSORTest])
    suite.addTestSuite(classOf[FloatChebyshevILUTest])
    suite.addTestSuite(classOf[FloatChebyshevICCTest])
    suite.addTestSuite(classOf[FloatChebyshevAMGTest])
    suite.addTestSuite(classOf[FloatChebyshevILUTTest])
    suite.addTestSuite(classOf[FloatIRDiagonalTest])
    suite.addTestSuite(classOf[FloatIRSSORTest])
    suite.addTestSuite(classOf[FloatIRILUTest])
    suite.addTestSuite(classOf[FloatIRICCTest])
    suite.addTestSuite(classOf[FloatIRAMGTest])
    suite.addTestSuite(classOf[FloatIRILUTTest])
    suite
  }
}
