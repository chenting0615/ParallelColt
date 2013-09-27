package cern.colt.matrix.tdcomplex.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCDComplexMatrix2DViewTest(arg0: String) extends SparseRCDComplexMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCDComplexMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCDComplexMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
