package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseCCMFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCMFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCMFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCMFloatMatrix2D(NCOLUMNS, NROWS)
  }
}
