package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseColumnDoubleMatrix2DViewTest(arg0: String) extends DenseColumnDoubleMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseColumnDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseColumnDoubleMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseColumnDoubleMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
