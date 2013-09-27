package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCLongMatrix2DViewTest(arg0: String) extends SparseRCLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
