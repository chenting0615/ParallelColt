package cern.colt.matrix.tint.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseIntMatrix2DViewTest(arg0: String) extends SparseIntMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseIntMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseIntMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
