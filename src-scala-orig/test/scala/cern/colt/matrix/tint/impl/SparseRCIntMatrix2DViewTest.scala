package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCIntMatrix2DViewTest(arg0: String) extends SparseRCIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
