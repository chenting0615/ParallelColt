package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMIntMatrix2DViewTest(arg0: String) extends SparseRCMIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCMIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCMIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
