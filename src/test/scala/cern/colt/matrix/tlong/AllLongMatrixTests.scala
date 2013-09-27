package cern.colt.matrix.tlong

import junit.framework.Test
import junit.framework.TestSuite
import cern.colt.matrix.tlong.impl.DenseColumnLongMatrix2DTest
import cern.colt.matrix.tlong.impl.DenseColumnLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.DenseLargeLongMatrix2DTest
import cern.colt.matrix.tlong.impl.DenseLargeLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.DenseLargeLongMatrix3DTest
import cern.colt.matrix.tlong.impl.DenseLargeLongMatrix3DViewTest
import cern.colt.matrix.tlong.impl.DenseLongMatrix1DTest
import cern.colt.matrix.tlong.impl.DenseLongMatrix1DViewTest
import cern.colt.matrix.tlong.impl.DenseLongMatrix2DTest
import cern.colt.matrix.tlong.impl.DenseLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.DenseLongMatrix3DTest
import cern.colt.matrix.tlong.impl.DenseLongMatrix3DViewTest
import cern.colt.matrix.tlong.impl.DiagonalLongMatrix2DTest
import cern.colt.matrix.tlong.impl.DiagonalLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.SparseCCLongMatrix2DTest
import cern.colt.matrix.tlong.impl.SparseCCLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.SparseCCMLongMatrix2DTest
import cern.colt.matrix.tlong.impl.SparseCCMLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.SparseLongMatrix1DTest
import cern.colt.matrix.tlong.impl.SparseLongMatrix1DViewTest
import cern.colt.matrix.tlong.impl.SparseLongMatrix2DTest
import cern.colt.matrix.tlong.impl.SparseLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.SparseLongMatrix3DTest
import cern.colt.matrix.tlong.impl.SparseLongMatrix3DViewTest
import cern.colt.matrix.tlong.impl.SparseRCLongMatrix2DTest
import cern.colt.matrix.tlong.impl.SparseRCLongMatrix2DViewTest
import cern.colt.matrix.tlong.impl.SparseRCMLongMatrix2DTest
import cern.colt.matrix.tlong.impl.SparseRCMLongMatrix2DViewTest
//remove if not needed
import scala.collection.JavaConversions._

object AllLongMatrixTests {

  def suite(): Test = {
    val suite = new TestSuite("cern.colt.matrix.tlong tests")
    suite.addTestSuite(classOf[DenseLongMatrix1DTest])
    suite.addTestSuite(classOf[DenseLongMatrix1DViewTest])
    suite.addTestSuite(classOf[SparseLongMatrix1DTest])
    suite.addTestSuite(classOf[SparseLongMatrix1DViewTest])
    suite.addTestSuite(classOf[DenseLongMatrix2DTest])
    suite.addTestSuite(classOf[DenseLongMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseColumnLongMatrix2DTest])
    suite.addTestSuite(classOf[DenseColumnLongMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseLargeLongMatrix2DTest])
    suite.addTestSuite(classOf[DenseLargeLongMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseLongMatrix2DTest])
    suite.addTestSuite(classOf[SparseLongMatrix2DViewTest])
    suite.addTestSuite(classOf[DiagonalLongMatrix2DTest])
    suite.addTestSuite(classOf[DiagonalLongMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCLongMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCLongMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseRCMLongMatrix2DTest])
    suite.addTestSuite(classOf[SparseRCMLongMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCLongMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCLongMatrix2DViewTest])
    suite.addTestSuite(classOf[SparseCCMLongMatrix2DTest])
    suite.addTestSuite(classOf[SparseCCMLongMatrix2DViewTest])
    suite.addTestSuite(classOf[DenseLongMatrix3DTest])
    suite.addTestSuite(classOf[DenseLongMatrix3DViewTest])
    suite.addTestSuite(classOf[SparseLongMatrix3DTest])
    suite.addTestSuite(classOf[SparseLongMatrix3DViewTest])
    suite.addTestSuite(classOf[DenseLargeLongMatrix3DTest])
    suite.addTestSuite(classOf[DenseLargeLongMatrix3DViewTest])
    suite
  }
}
