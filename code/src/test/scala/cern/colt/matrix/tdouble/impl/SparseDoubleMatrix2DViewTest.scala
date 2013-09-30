package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class SparseDoubleMatrix2DViewTest(arg0: String) extends SparseDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new SparseDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new SparseDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new SparseDoubleMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
