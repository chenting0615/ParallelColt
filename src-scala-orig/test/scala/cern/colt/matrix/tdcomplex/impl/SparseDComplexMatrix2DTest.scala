package cern.colt.matrix.tdcomplex.impl

import cern.colt.matrix.tdcomplex.DComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseDComplexMatrix2DTest(arg0: String) extends DComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDComplexMatrix2D(NROWS, NCOLUMNS)
    B = new SparseDComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseDComplexMatrix2D(NCOLUMNS, NROWS)
  }
}
