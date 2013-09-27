package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseRCDoubleMatrix2DViewTest(arg0: String) extends SparseRCDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseRCDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseRCDoubleMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
