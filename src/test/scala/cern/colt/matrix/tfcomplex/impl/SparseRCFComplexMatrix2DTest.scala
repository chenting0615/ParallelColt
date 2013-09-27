package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseRCFComplexMatrix2DTest(arg0: String) extends FComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCFComplexMatrix2D(NROWS, NCOLUMNS)
    B = new SparseRCFComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseRCFComplexMatrix2D(NCOLUMNS, NROWS)
  }
}
