package cern.colt.matrix.tdouble

import junit.framework.Test
import junit.framework.TestSuite
import cern.colt.matrix.tdouble.algo.solver.AllDoubleMatrixSolverTests
import cern.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.DenseColumnDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix1DTest
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix1DViewTest
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix3DTest
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix3DViewTest
import cern.colt.matrix.tdouble.impl.DenseLargeDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.DenseLargeDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.DenseLargeDoubleMatrix3DTest
import cern.colt.matrix.tdouble.impl.DenseLargeDoubleMatrix3DViewTest
import cern.colt.matrix.tdouble.impl.DiagonalDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.DiagonalDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.SparseCCDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.SparseCCMDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.SparseCCMDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix1DTest
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix1DViewTest
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix3DTest
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix3DViewTest
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.SparseRCDoubleMatrix2DViewTest
import cern.colt.matrix.tdouble.impl.SparseRCMDoubleMatrix2DTest
import cern.colt.matrix.tdouble.impl.SparseRCMDoubleMatrix2DViewTest
//remove if not needed
import scala.collection.JavaConversions._

object AllDoubleMatrixTests {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tdouble tests")
    suite.addTestSuite(classOf[DenseDoubleMatrix1DTest])
    suite.addTestSuite(classOf[DenseDoubleMatrix1DViewTest])
    suite.addTestSuite(classOf[SparseDoubleMatrix1DTest])
    suite.addTestSuite(classOf[SparseDoubleMatrix1DViewTest])
    suite.addTestSuite(classOf[DenseDoubleMatrix2DTest])
    suite.addTestSuite(classOf[DenseDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseColumnDoubleMatrix2DTest])
    suite.addTestSuite(classOf[DenseColumnDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseLargeDoubleMatrix2DTest])
    suite.addTestSuite(classOf[DenseLargeDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseDoubleMatrix2DTest])
    suite.addTestSuite(classOf[SparseDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[DiagonalDoubleMatrix2DTest])
    suite.addTestSuite(classOf[DiagonalDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCDoubleMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCMDoubleMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCMDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCDoubleMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCMDoubleMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCMDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseDoubleMatrix3DTest])
    suite.addTestSuite(classOf[DenseDoubleMatrix3DViewTest])
    suite.addTestSuite(classOf[SparseDoubleMatrix3DTest])
    suite.addTestSuite(classOf[SparseDoubleMatrix3DViewTest])
    suite.addTestSuite(classOf[DenseLargeDoubleMatrix3DTest])
    suite.addTestSuite(classOf[DenseLargeDoubleMatrix3DViewTest])
    suite.addTest(AllDoubleMatrixSolverTests.suite())
    suite
  }
}
