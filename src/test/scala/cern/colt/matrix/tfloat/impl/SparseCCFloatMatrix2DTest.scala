package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
import cern.colt.matrix.MatrixTypes.SparseCCFloatMatrix2D

class SparseCCFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  override protected def createMatrices() {
    A = new SparseCCFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS)
  }
}
