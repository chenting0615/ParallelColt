package cern.colt.matrix.tfloat.impl

import cern.colt.matrix.tfloat.FloatMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseCCFloatMatrix2DTest(arg0: String) extends FloatMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCFloatMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCFloatMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCFloatMatrix2D(NCOLUMNS, NROWS)
  }
}
