package cern.colt.matrix.tfcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCFComplexMatrix2DViewTest(arg0: String) extends SparseRCFComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCFComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCFComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
