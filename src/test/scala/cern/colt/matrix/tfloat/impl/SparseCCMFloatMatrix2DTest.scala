package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.colt.matrix.MatrixTypes.SparseCCMFloatMatrix2D

class SparseCCMFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCMFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCMFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS)
  }
}
