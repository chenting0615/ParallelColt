package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCMDoubleMatrix2DViewTest(arg0: String) extends SparseRCMDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCMDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCMDoubleMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
