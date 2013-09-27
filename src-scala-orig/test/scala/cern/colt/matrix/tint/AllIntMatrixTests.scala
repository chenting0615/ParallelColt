package cern.colt.matrix.tint

import junit.framework.Test
import junit.framework.TestSuite
import cern.colt.matrix.tint.impl.DenseColumnIntMatrix2DTest
import cern.colt.matrix.tint.impl.DenseColumnIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.DenseIntMatrix1DTest
import cern.colt.matrix.tint.impl.DenseIntMatrix1DViewTest
import cern.colt.matrix.tint.impl.DenseIntMatrix2DTest
import cern.colt.matrix.tint.impl.DenseIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.DenseIntMatrix3DTest
import cern.colt.matrix.tint.impl.DenseIntMatrix3DViewTest
import cern.colt.matrix.tint.impl.DenseLargeIntMatrix2DTest
import cern.colt.matrix.tint.impl.DenseLargeIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.DenseLargeIntMatrix3DTest
import cern.colt.matrix.tint.impl.DenseLargeIntMatrix3DViewTest
import cern.colt.matrix.tint.impl.DiagonalIntMatrix2DTest
import cern.colt.matrix.tint.impl.DiagonalIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.SparseCCIntMatrix2DTest
import cern.colt.matrix.tint.impl.SparseCCIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.SparseCCMIntMatrix2DTest
import cern.colt.matrix.tint.impl.SparseCCMIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.SparseIntMatrix1DTest
import cern.colt.matrix.tint.impl.SparseIntMatrix1DViewTest
import cern.colt.matrix.tint.impl.SparseIntMatrix2DTest
import cern.colt.matrix.tint.impl.SparseIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.SparseIntMatrix3DTest
import cern.colt.matrix.tint.impl.SparseIntMatrix3DViewTest
import cern.colt.matrix.tint.impl.SparseRCIntMatrix2DTest
import cern.colt.matrix.tint.impl.SparseRCIntMatrix2DViewTest
import cern.colt.matrix.tint.impl.SparseRCMIntMatrix2DTest
import cern.colt.matrix.tint.impl.SparseRCMIntMatrix2DViewTest
//remove if not needed
import scala.collection.JavaConversions._

object AllIntMatrixTests {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tint tests")
    suite.addTestSuite(classOf[DenseIntMatrix1DTest])
    suite.addTestSuite(classOf[DenseIntMatrix1DViewTest])
    suite.addTestSuite(classOf[SparseIntMatrix1DTest])
    suite.addTestSuite(classOf[SparseIntMatrix1DViewTest])
    suite.addTestSuite(classOf[DenseIntMatrix2DTest])
    suite.addTestSuite(classOf[DenseIntMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseColumnIntMatrix2DTest])
    suite.addTestSuite(classOf[DenseColumnIntMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseLargeIntMatrix2DTest])
    suite.addTestSuite(classOf[DenseLargeIntMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseIntMatrix2DTest])
    suite.addTestSuite(classOf[SparseIntMatrix2DViewTest])
    suite.addTestSuite(classOf[DiagonalIntMatrix2DTest])
    suite.addTestSuite(classOf[DiagonalIntMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCIntMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCIntMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCMIntMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCMIntMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCIntMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCIntMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCMIntMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCMIntMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseIntMatrix3DTest])
    suite.addTestSuite(classOf[DenseIntMatrix3DViewTest])
    suite.addTestSuite(classOf[SparseIntMatrix3DTest])
    suite.addTestSuite(classOf[SparseIntMatrix3DViewTest])
    suite.addTestSuite(classOf[DenseLargeIntMatrix3DTest])
    suite.addTestSuite(classOf[DenseLargeIntMatrix3DViewTest])
    suite
  }
}
