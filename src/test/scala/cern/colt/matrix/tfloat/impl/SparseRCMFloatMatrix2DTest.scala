package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.colt.matrix.MatrixTypes.SparseRCMFloatMatrix2D

class SparseRCMFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCMFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCMFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCMFloatMatrix2D(NCOLUMNS, NROWS)
  }
}
