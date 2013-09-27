package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseRCFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCFloatMatrix2D(NCOLUMNS, NROWS)
  }
}
