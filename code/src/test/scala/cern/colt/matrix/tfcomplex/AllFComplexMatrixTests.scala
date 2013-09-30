package cern.colt.matrix.tfcomplex

import junit.framework.Test
import junit.framework.TestSuite
import cern.colt.matrix.tfcomplex.impl.DenseColumnFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.DenseColumnFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.DenseFComplexMatrix1DTest
import cern.colt.matrix.tfcomplex.impl.DenseFComplexMatrix1DViewTest
import cern.colt.matrix.tfcomplex.impl.DenseFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.DenseFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.DenseFComplexMatrix3DTest
import cern.colt.matrix.tfcomplex.impl.DenseFComplexMatrix3DViewTest
import cern.colt.matrix.tfcomplex.impl.DiagonalFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.DiagonalFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.LargeDenseFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.LargeDenseFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.LargeDenseFComplexMatrix3DTest
import cern.colt.matrix.tfcomplex.impl.LargeDenseFComplexMatrix3DViewTest
import cern.colt.matrix.tfcomplex.impl.SparseCCFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.SparseCCFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.SparseCCMFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.SparseCCMFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.SparseFComplexMatrix1DTest
import cern.colt.matrix.tfcomplex.impl.SparseFComplexMatrix1DViewTest
import cern.colt.matrix.tfcomplex.impl.SparseFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.SparseFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.SparseFComplexMatrix3DTest
import cern.colt.matrix.tfcomplex.impl.SparseFComplexMatrix3DViewTest
import cern.colt.matrix.tfcomplex.impl.SparseRCFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.SparseRCFComplexMatrix2DViewTest
import cern.colt.matrix.tfcomplex.impl.SparseRCMFComplexMatrix2DTest
import cern.colt.matrix.tfcomplex.impl.SparseRCMFComplexMatrix2DViewTest
//remove if not needed
import scala.collection.JavaConversions._

object AllFComplexMatrixTests {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tfcomplex tests")
    suite.addTestSuite(classOf[DenseFComplexMatrix1DTest])
    suite.addTestSuite(classOf[DenseFComplexMatrix1DViewTest])
    suite.addTestSuite(classOf[SparseFComplexMatrix1DTest])
    suite.addTestSuite(classOf[SparseFComplexMatrix1DViewTest])
    suite.addTestSuite(classOf[DenseFComplexMatrix2DTest])
    suite.addTestSuite(classOf[DenseFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseColumnFComplexMatrix2DTest])
    suite.addTestSuite(classOf[DenseColumnFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[LargeDenseFComplexMatrix2DTest])
    suite.addTestSuite(classOf[LargeDenseFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCFComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCFComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCMFComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCMFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCMFComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCMFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseFComplexMatrix2DTest])
    suite.addTestSuite(classOf[SparseFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[DiagonalFComplexMatrix2DTest])
    suite.addTestSuite(classOf[DiagonalFComplexMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseFComplexMatrix3DTest])
    suite.addTestSuite(classOf[DenseFComplexMatrix3DViewTest])
    suite.addTestSuite(classOf[SparseFComplexMatrix3DTest])
    suite.addTestSuite(classOf[SparseFComplexMatrix3DViewTest])
    suite.addTestSuite(classOf[LargeDenseFComplexMatrix3DTest])
    suite.addTestSuite(classOf[LargeDenseFComplexMatrix3DViewTest])
    suite
  }
}
