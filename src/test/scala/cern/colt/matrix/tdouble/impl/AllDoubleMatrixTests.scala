package cern.colt.matrix.tdouble.impl

import junit.framework.{TestCase, Test, TestSuite}

object AllDoubleMatrixTests extends TestCase {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tdouble tests")
    suite.addTestSuite(classOf[DenseDoubleMatrix1DTest])
    suite.addTestSuite(classOf[DenseDoubleMatrix1DViewTest])
    suite.addTestSuite(classOf[SparseMatrix1DTest])
    suite.addTestSuite(classOf[SparseDoubleMatrix1DViewTest])
    suite.addTestSuite(classOf[DenseMatrix2DTest])
    suite.addTestSuite(classOf[DenseDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseColumnDoubleMatrix2DTest])
    suite.addTestSuite(classOf[DenseColumnDoubleMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseLargeMatrix2DTest])
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
    suite
  }
}
