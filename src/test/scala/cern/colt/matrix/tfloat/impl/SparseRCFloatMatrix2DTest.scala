package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.colt.matrix.MatrixTypes.SparseRCFloatMatrix2D

class SparseRCFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseRCFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS)
  }
}
