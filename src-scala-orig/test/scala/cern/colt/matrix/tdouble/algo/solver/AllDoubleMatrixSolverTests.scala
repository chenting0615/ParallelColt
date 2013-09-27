package cern.colt.matrix.tdouble.algo.solver

import junit.framework.Test
import junit.framework.TestSuite
//remove if not needed
import scala.collection.JavaConversions._

object AllDoubleMatrixSolverTests {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tdouble.algo.solvers tests")
    suite.addTestSuite(classOf[DoubleCGTest])
    suite.addTestSuite(classOf[DoubleCGDiagonalTest])
    suite.addTestSuite(classOf[DoubleCGSSORTest])
    suite.addTestSuite(classOf[DoubleCGILUTest])
    suite.addTestSuite(classOf[DoubleCGICCTest])
    suite.addTestSuite(classOf[DoubleCGILUTTest])
    suite.addTestSuite(classOf[DoubleCGSTest])
    suite.addTestSuite(classOf[DoubleCGSDiagonalTest])
    suite.addTestSuite(classOf[DoubleCGSSSORTest])
    suite.addTestSuite(classOf[DoubleCGSILUTest])
    suite.addTestSuite(classOf[DoubleCGSICCTest])
    suite.addTestSuite(classOf[DoubleCGSILUTTest])
    suite.addTestSuite(classOf[DoubleQMRTest])
    suite.addTestSuite(classOf[DoubleQMRDiagonalTest])
    suite.addTestSuite(classOf[DoubleQMRSSORTest])
    suite.addTestSuite(classOf[DoubleQMRILUTest])
    suite.addTestSuite(classOf[DoubleQMRICCTest])
    suite.addTestSuite(classOf[DoubleQMRILUTTest])
    suite.addTestSuite(classOf[DoubleBiCGTest])
    suite.addTestSuite(classOf[DoubleBiCGDiagonalTest])
    suite.addTestSuite(classOf[DoubleBiCGSSORTest])
    suite.addTestSuite(classOf[DoubleBiCGILUTest])
    suite.addTestSuite(classOf[DoubleBiCGICCTest])
    suite.addTestSuite(classOf[DoubleBiCGAMGTest])
    suite.addTestSuite(classOf[DoubleBiCGILUTTest])
    suite.addTestSuite(classOf[DoubleBiCGstabTest])
    suite.addTestSuite(classOf[DoubleBiCGstabDiagonalTest])
    suite.addTestSuite(classOf[DoubleBiCGstabSSORTest])
    suite.addTestSuite(classOf[DoubleBiCGstabILUTest])
    suite.addTestSuite(classOf[DoubleBiCGstabICCTest])
    suite.addTestSuite(classOf[DoubleBiCGstabAMGTest])
    suite.addTestSuite(classOf[DoubleBiCGstabILUTTest])
    suite.addTestSuite(classOf[DoubleGMRESTest])
    suite.addTestSuite(classOf[DoubleGMRESDiagonalTest])
    suite.addTestSuite(classOf[DoubleGMRESSSORTest])
    suite.addTestSuite(classOf[DoubleGMRESILUTest])
    suite.addTestSuite(classOf[DoubleGMRESICCTest])
    suite.addTestSuite(classOf[DoubleGMRESAMGTest])
    suite.addTestSuite(classOf[DoubleGMRESILUTTest])
    suite.addTestSuite(classOf[DoubleChebyshevTest])
    suite.addTestSuite(classOf[DoubleChebyshevDiagonalTest])
    suite.addTestSuite(classOf[DoubleChebyshevSSORTest])
    suite.addTestSuite(classOf[DoubleChebyshevILUTest])
    suite.addTestSuite(classOf[DoubleChebyshevICCTest])
    suite.addTestSuite(classOf[DoubleChebyshevAMGTest])
    suite.addTestSuite(classOf[DoubleChebyshevILUTTest])
    suite.addTestSuite(classOf[DoubleIRDiagonalTest])
    suite.addTestSuite(classOf[DoubleIRSSORTest])
    suite.addTestSuite(classOf[DoubleIRILUTest])
    suite.addTestSuite(classOf[DoubleIRICCTest])
    suite.addTestSuite(classOf[DoubleIRAMGTest])
    suite.addTestSuite(classOf[DoubleIRILUTTest])
    suite
  }
}
