package cern.colt.matrix.tfcomplex.impl

import cern.colt.matrix.tfcomplex.FComplexMatrix2DTest
//remove if not needed
import scala.collection.JavaConversions._

class SparseCCFComplexMatrix2DTest(arg0: String) extends FComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseCCFComplexMatrix2D(NROWS, NCOLUMNS)
    B = new SparseCCFComplexMatrix2D(NROWS, NCOLUMNS)
    Bt = new SparseCCFComplexMatrix2D(NCOLUMNS, NROWS)
  }
}
