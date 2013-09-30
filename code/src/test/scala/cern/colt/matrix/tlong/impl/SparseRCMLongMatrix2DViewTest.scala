package cern.colt.matrix.tlong.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMLongMatrix2DViewTest(arg0: String) extends SparseRCMLongMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCMLongMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCMLongMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
