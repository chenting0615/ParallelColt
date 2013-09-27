package cern.colt.matrix.tdcomplex

import junit.framework.Test
import junit.framework.TestSuite
import cern.colt.matrix.tdcomplex.impl.DenseColumnDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.DenseColumnDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix1DTest
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix1DViewTest
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix3DTest
import cern.colt.matrix.tdcomplex.impl.DenseDComplexMatrix3DViewTest
import cern.colt.matrix.tdcomplex.impl.DiagonalDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.DiagonalDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.LargeDenseDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.LargeDenseDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.LargeDenseDComplexMatrix3DTest
import cern.colt.matrix.tdcomplex.impl.LargeDenseDComplexMatrix3DViewTest
import cern.colt.matrix.tdcomplex.impl.SparseCCDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.SparseCCDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.SparseCCMDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.SparseCCMDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.SparseDComplexMatrix1DTest
import cern.colt.matrix.tdcomplex.impl.SparseDComplexMatrix1DViewTest
import cern.colt.matrix.tdcomplex.impl.SparseDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.SparseDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.SparseDComplexMatrix3DTest
import cern.colt.matrix.tdcomplex.impl.SparseDComplexMatrix3DViewTest
import cern.colt.matrix.tdcomplex.impl.SparseRCDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.SparseRCDComplexMatrix2DViewTest
import cern.colt.matrix.tdcomplex.impl.SparseRCMDComplexMatrix2DTest
import cern.colt.matrix.tdcomplex.impl.SparseRCMDComplexMatrix2DViewTest
//remove if not needed
import scala.collection.JavaConversions._

object AllDComplexMatrixTests {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tdcomplex tests")
    suite.addTestSuite(classOf[DenseDComplexMatrix1DTest])
    suite.addTestSuite(classOf[DenseDComplexMatrix1DViewTest])
    suite.addTestSuite(classOf[SparseDComplexMatrix1DTest])
    suite.addTestSuite(classOf[SparseDComplexMatrix1DViewTest])
    suite.addTestSuite(classOf[DenseDComplexMatrix2DTest])
    suite.addTestSuite(classOf[DenseDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseColumnDComplexMatrix2DTest])
    suite.addTestSuite(classOf[DenseColumnDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[LargeDenseDComplexMatrix2DTest])
    suite.addTestSuite(classOf[LargeDenseDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseDComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCDComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCMDComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCMDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCDComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCMDComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCMDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[DiagonalDComplexMatrix2DTest])
    suite.addTestSuite(classOf[DiagonalDComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseDComplexMatrix3DTest])
    suite.addTestSuite(classOf[DenseDComplexMatrix3DViewTest])
    suite.addTestSuite(classOf[SparseDComplexMatrix3DTest])
    suite.addTestSuite(classOf[SparseDComplexMatrix3DViewTest])
    suite.addTestSuite(classOf[LargeDenseDComplexMatrix3DTest])
    suite.addTestSuite(classOf[LargeDenseDComplexMatrix3DViewTest])
    suite
  }
}
