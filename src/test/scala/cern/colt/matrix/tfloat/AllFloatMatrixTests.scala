package cern.colt.matrix.tfloat

import junit.framework.{TestCase, Test, TestSuite}
import cern.colt.matrix.tfloat.impl.DenseColumnFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.DenseColumnFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.DenseFloatMatrix1DTest
import cern.colt.matrix.tfloat.impl.DenseFloatMatrix1DViewTest
import cern.colt.matrix.tfloat.impl.DenseFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.DenseFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.DenseLargeFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.DenseLargeFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.DiagonalFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.DiagonalFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.SparseCCFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.SparseCCFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.SparseCCMFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.SparseCCMFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.SparseFloatMatrix1DTest
import cern.colt.matrix.tfloat.impl.SparseFloatMatrix1DViewTest
import cern.colt.matrix.tfloat.impl.SparseFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.SparseFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.SparseRCFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.SparseRCFloatMatrix2DViewTest
import cern.colt.matrix.tfloat.impl.SparseRCMFloatMatrix2DTest
import cern.colt.matrix.tfloat.impl.SparseRCMFloatMatrix2DViewTest

object AllFloatMatrixTests extends TestCase {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tfloat tests")
    suite.addTestSuite(classOf[DenseFloatMatrix1DTest])
    suite.addTestSuite(classOf[DenseFloatMatrix1DViewTest])
    suite.addTestSuite(classOf[SparseFloatMatrix1DTest])
    suite.addTestSuite(classOf[SparseFloatMatrix1DViewTest])
    suite.addTestSuite(classOf[DenseFloatMatrix2DTest])
    suite.addTestSuite(classOf[DenseFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseColumnFloatMatrix2DTest])
    suite.addTestSuite(classOf[DenseColumnFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseLargeFloatMatrix2DTest])
    suite.addTestSuite(classOf[DenseLargeFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseFloatMatrix2DTest])
    suite.addTestSuite(classOf[SparseFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[DiagonalFloatMatrix2DTest])
    suite.addTestSuite(classOf[DiagonalFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCFloatMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCMFloatMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCMFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCFloatMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCFloatMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCMFloatMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCMFloatMatrix2DViewTest])
//    suite.addTestSuite(classOf[DenseFloatMatrix3DTest])
//    suite.addTestSuite(classOf[DenseFloatMatrix3DViewTest])
//    suite.addTestSuite(classOf[SparseFloatMatrix3DTest])
//    suite.addTestSuite(classOf[SparseFloatMatrix3DViewTest])
//    suite.addTestSuite(classOf[DenseLargeFloatMatrix3DTest])
//    suite.addTestSuite(classOf[DenseLargeFloatMatrix3DViewTest])
    suite
  }
}
