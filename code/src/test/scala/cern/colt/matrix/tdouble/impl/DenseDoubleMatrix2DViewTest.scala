package cern.colt.matrix.tdouble.impl

//remove if not needed
import scala.collection.JavaConversions._

class DenseDoubleMatrix2DViewTest(arg0: String) extends DenseMatrix2DTest(arg0) {

  protected def createMatrices() {
    A = new DenseMatrix2D(NCOLUMNS, NROWS).viewDice()
    B = new DenseMatrix2D(NCOLUMNS, NROWS).viewDice()
    Bt = new DenseMatrix2D(NROWS, NCOLUMNS).viewDice()
  }
}
